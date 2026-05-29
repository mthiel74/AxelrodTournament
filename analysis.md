# Analysis — Axelrod tournament, evolution, and strategy discovery

All numbers below are produced by the scripts in `wolfram/` and the data in
`data/`. Move encoding: `C = 1`, `D = 0`. Payoffs `T=5 > R=3 > P=1 > S=0`. Noise
`ε` is execution error (each intended move flips with probability `ε`).

## 1. The classic round-robin (Axelrod 1980 reproduction)

Round-robin among 13 strategies, 200 rounds/match, 100 repetitions, self-play
included. Each strategy's score is its **mean per-round payoff against the whole
field**. (`wolfram/tournament.wls` → `data/ranking_noise*.csv`.)

**At ε = 0 (no noise):**

| rank | strategy     | score |
|------|--------------|-------|
| 1 | GenerousTFT   | 2.659 |
| 2 | ZD-Generous   | 2.656 |
| 3 | TitFor2Tats   | 2.624 |
| 4 | TitForTat     | 2.602 |
| 5 | AllC          | 2.597 |
| 6 | Pavlov        | 2.569 |
| … | …             | …     |
| 12 | AllD         | 1.800 |
| 13 | ZD-Extort-2   | 1.712 |

The classic lesson holds: **nice, reciprocal, forgiving** strategies dominate a
cooperative field. Plain Tit-for-Tat is excellent but is *edged out by its more
forgiving cousins* (Generous TFT, ZD-Generous, Tit-for-2-Tats) — exactly the
refinement Axelrod found in his second tournament. The extortioner finishes
**last**: extortion wins individual matches but poisons the well it drinks from.

**As noise rises** (ε = 0.01 → 0.1), the ordering reshuffles. Unforgiving
strategies (Grim, Gradual, 2TitsForTat) tumble down the table because a single
*accidental* defection triggers retaliation spirals they cannot exit; tolerant
strategies (TitFor2Tats, GenerousTFT) stay on top. By ε = 0.1 the whole field is
compressed toward the mutual-defection payoff `P = 1`, and the gap between "nice"
and "nasty" strategies has largely closed. See `docs/images/score_vs_noise.png`
and `docs/images/ranking_grid.png`.

## 2. The evolutionary tournament — the stable mix depends on noise

We let the population of strategies adapt under two dynamics driven by the same
mean-payoff matrix `A` (`A[[i,j]]` = mean score of `i` vs `j`):

- **Replicator dynamics** (deterministic): `xᵢ ← xᵢ · (A·x)ᵢ / (x·A·x)`, with a
  tiny mutation term keeping the simplex interior. *Headline result.*
- **Moran process** (stochastic, finite `N=100`): frequency-dependent birth–death
  with rare mutation; we report the long-run average composition. *Robustness.*

(`wolfram/evolution.wls` → `data/replicator_vs_noise.csv`,
`data/moran_vs_noise.csv`; figure `docs/images/replicator_vs_noise.png`.)

**The key finding.** The evolutionarily stable composition is a *function of the
noise level*:

- **Low noise (ε ≈ 0):** a **coexistence of cooperative strategies** — the
  forgiving reciprocators (Generous TFT, Tit-for-2-Tats, ZD-Generous, Pavlov,
  Tit-for-Tat) share the population in roughly equal measure. Cooperation is
  evolutionarily robust: mutual-cooperation payoffs sustain the whole cluster,
  and defectors cannot invade because the reciprocators punish them.
- **High noise (ε ≳ 0.15):** **cooperation collapses** — `AllD` sweeps the
  population (→ ~0.99 at ε = 0.2). Under heavy execution error, reciprocators
  spend so much time accidentally punishing each other that their cooperative
  advantage evaporates, and unconditional defection becomes the only stable
  attractor.

The transition between these regimes is the headline figure. The Moran process
tells the same qualitative story with finite-population stochasticity, confirming
the replicator result is not an artefact of the deterministic limit.

> **Takeaway:** "Which strategy is evolutionarily stable?" has *no
> noise-independent answer*. The same population that converges on generous
> reciprocity in a clean channel converges on pure defection in a noisy one.

## 3. Automated strategy discovery — beating Tit-for-Tat under noise

In the spirit of an automated-research loop (*propose → evaluate → accept →
repeat*; cf. Karpathy's `autoresearch`, re-implemented natively in WL — see
[`program.md`](program.md)), we search the memory-one strategy space
`θ = (open, pCC, pCD, pDC, pDD) ∈ [0,1]⁵` for the **best response to the standard
field at a given noise level**. A greedy hill-climb provides the narrative log
(`data/discovery_log_eps*.tsv`); a genetic search is the workhorse for the noise
sweep. (`wolfram/discover.wls` → `data/discovered_strategies.csv`.)

The discovered strategy **beats plain Tit-for-Tat at every noise level**:

| ε    | discovered field score | TFT field score | improvement |
|------|------------------------|-----------------|-------------|
| 0.00 | 2.665 | 2.600 | +0.064 |
| 0.05 | 2.193 | 2.061 | +0.132 |
| 0.10 | 2.136 | 2.080 | +0.056 |
| 0.15 | 2.167 | 2.121 | +0.045 |
| 0.20 | 2.224 | 2.104 | +0.120 |

And — the mechanistic point — the *shape* of the optimal strategy drifts with
noise (`docs/images/discovered_params_vs_noise.png`):

- At **ε = 0** the optimum is reciprocal and only mildly forgiving:
  `open≈1, pCC≈1, pCD≈0.23, pDC≈0.97, pDD≈0.17` — a slightly-generous TFT that
  also exploits pushovers (low `pDD`, i.e. keep defecting against a sucker).
- At **moderate noise** `pCD` (the probability of forgiving after being suckered)
  rises sharply — error-correction becomes valuable.
- At **high noise (ε = 0.2)** the best response against *this* field tilts toward
  defection (`open≈0.29, pCC≈0.07`): when everyone is drowning in noise, the
  field itself is decaying toward defection, and the best response decays with it.

This is the *same lesson the evolutionary tournament teaches*, reached by a
different route: the optimal behaviour is a function of the environment, and noise
is the environmental knob that turns generous reciprocity into defection.

## Reproducing

```bash
wolframscript -file tests/sanity.wls         # correctness checks
wolframscript -file wolfram/tournament.wls   # round-robin → data/ranking_*, matrix_*
wolframscript -file wolfram/evolution.wls    # replicator + Moran → data/*_vs_noise.csv
wolframscript -file wolfram/discover.wls      # discovery → data/discovered_strategies.csv
wolframscript -file wolfram/figures.wls       # all figures → docs/images/*.png
```

## Caveats / honest limitations

- Scores are Monte-Carlo estimates; ZD enforced relations hold to ~0.01–0.05
  (finite-sample noise), and rankings near ties can wobble between runs.
- The evolutionary field is the *fixed set of 13 named strategies*; replicator
  dynamics here select among them, they do not synthesise new ones. Strategy
  *synthesis* is the job of §3.
- The discovery search is modest (small populations / few generations) — it is a
  demonstration of the loop, not an exhaustive optimum. Scaling `reps`,
  population size, and generations sharpens the discovered strategies.
