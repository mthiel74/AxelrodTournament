# 8-hour autoresearch run — report

**Run:** 2026-06-03 23:04 → 2026-06-04 ~08:00 local. Supervised, zombie-safe
(`supervise.sh`), 111 generations completed, exited cleanly (no leaked kernels).

## What was searched

The Karpathy-style *propose → evaluate → keep* loop, scaled up to run for hours,
over a **memory-two stochastic strategy space** `θ = (open, p1..p16) ∈ [0,1]^17`.
This space strictly contains memory-one (TFT, Pavlov, Generous TFT, …) and, by
conditioning on the **last two** rounds, can express behaviours memory-one
cannot — most importantly *"distinguish an isolated mistake from sustained
defection."*

- **Engine:** 4-island genetic algorithm, ring migration, soft-restart on
  stagnation; hall of fame of 14.
- **Fitness (search):** mean per-round score against the fixed 18-strategy field
  (13 classic + 5 modern), averaged over noise ε ∈ {0, .02, .05, .1, .15, .2},
  deterministic (fixed seed) so candidates are comparable.
- **Final re-evaluation:** every hall-of-fame strategy re-scored at higher rep
  with **fresh** random seeds, to filter strategies that merely overfit the
  search seed.

## Headline result

| | sweep score | vs TFT |
|---|---|---|
| **Discovered champion (memory-two)** | **2.498** | **+0.211** |
| Tit-for-Tat (baseline) | 2.287 | — |

**Independent validation** (4 fresh seeds never used during the run, reps=25):
champion **2.494** vs TFT **2.292** → **+0.202 (+8.8%)**. The improvement is
real, not an artefact of the search seed.

The search converged early (best plateaued by generation ~75); the later
generations confirmed the plateau rather than improving on it.

## What the champion actually does

Opening: cooperate with P ≈ 0.90. The interesting behaviour is two-round
memory (probabilities of cooperating next, averaged over the older round):

- Opponent **cooperated last round** → cooperate (it reciprocates cooperation).
- Opponent **defected last round but cooperated the round before** (an isolated,
  now-corrected blip — exactly what execution noise produces) → **forgive at
  ≈ 0.90**.
- Opponent **defected both of the last two rounds** (a sustained defector) →
  **cooperate only ≈ 0.21** (it punishes persistence).

That conditional — *forgive a one-off defection, punish a repeated one* — is the
crux. A memory-one strategy sees only the last move and must pick a single
forgiveness probability for "opponent just defected"; the memory-two champion
splits that case by whether the defection looks accidental or deliberate. This
is precisely the structure that pays off under noise, and it is why the gain
over TFT is largest at low-to-moderate noise where mistakes are recoverable.

Head-to-head (200 rounds, ε=0) it reaches full mutual cooperation (600–600) with
AllC, TFT, Grim, Pavlov, and itself, while resisting AllD (189 vs 244 — it stops
being exploited after the first couple of rounds).

## Files (in `autoresearch/long/`)

- `champion_robust.wl` — the champion as a runnable strategy.
- `champion_robust_meta.txt` — genome + headline numbers.
- `final_report.tsv` — all 14 hall-of-fame strategies, robust sweep, per-noise breakdown.
- `hall_of_fame.tsv` — the 14 best genomes found.
- `progress.tsv` — best-sweep trajectory over generations.

## Operational note

This run was managed by `supervise.sh` + `reap_zombies.sh`, which cap the
wall-clock budget and guarantee no orphaned `WolframKernel` is left spinning
after the job ends (the failure mode that previously crashed the machine). It
exited with zero leaked kernels.
