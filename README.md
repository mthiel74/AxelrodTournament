# 🤝 Axelrod Tournament — Iterated Prisoner's Dilemma in the Wolfram Language

A reproduction of **Robert Axelrod's classic 1980 round-robin tournament**, an
**evolutionary tournament** under replicator and Moran dynamics, an **automated
strategy-discovery loop**, and **five advanced extensions** — all in the Wolfram
Language.

When self-interested agents meet again and again, which behaviour wins? Axelrod's
1980 answer — *Tit-for-Tat* — became folklore. Here we reproduce it, then push
well past it: we let strategy populations **evolve**, show the evolutionarily
stable mix **depends on the level of noise**, let optimisers **discover** new
strategies that beat Tit-for-Tat, and finally explore spatial structure,
co-evolution, and evolutionary stability.

## 🎯 The base project

1. **Classic tournament.** A round-robin among classic strategies (Tit-for-Tat,
   Pavlov/Win-Stay-Lose-Shift, Grim, Generous Tit-for-Tat, …) plus modern
   entrants (zero-determinant extortion & generosity, Gradual), under the
   standard payoffs `T=5 > R=3 > P=1 > S=0`.
2. **Evolutionary tournament.** Strategy populations adapt via **replicator
   dynamics** (deterministic) and a **Moran process** (stochastic). Sweeping the
   **noise level** shows the stable mix shifts qualitatively.
3. **Strategy discovery.** A native-WL *propose → evaluate → accept* loop (greedy
   hill-climb + genetic search over memory-one space) finds best-response
   strategies that beat Tit-for-Tat — and the optimum drifts from *reciprocal*
   to *forgiving* as noise rises.

## 🚀 Going further — five advanced extensions

4. **Autoresearch champions** (`autoresearch/`) — a faithful port of Karpathy's
   *autoresearch* loop (propose → evaluate → keep; an AI editing one solution
   file scored by a fixed harness) to the IPD domain. 7 strategies beat the
   Tit-for-Tat baseline; the best is **Tit-for-2-Tats**, and a novel self-tuning
   **AdaptiveGenerous** (forgiveness scaled to the estimated noise) also wins.
5. **Finite-state-machine / genetic programming** (`wolfram/fsm_discover.wls`) —
   evolve Mealy-machine strategies *beyond memory-one*; the engine is verified to
   encode TFT and Grim exactly.
6. **Spatial evolution** (`wolfram/spatial.wls`) — strategies on a 32×32 lattice
   with Fermi imitation. **Clustering makes cooperation far more robust to noise**
   than in a well-mixed population (it degrades gracefully instead of collapsing).
7. **Red-Queen co-evolution** (`wolfram/coevolution.wls`) — the strategy
   population co-evolves against *itself*, with no fixed benchmark.
8. **ESS invasion analysis** (`wolfram/ess.wls`) — pairwise invasibility over a
   25-strategy panel; which strategies are uninvadable, as a function of noise.

## 🧬 Model

- **Move encoding:** `C = 1` (cooperate), `D = 0` (defect).
- **A strategy** is a pure function `s[myHist, oppHist] -> 0|1` over chronological
  histories of *actual* moves.
- **Noise (trembling hand):** each intended move is flipped with probability `ε`;
  opponents observe the *actual* (post-noise) move — the standard execution-error
  model.
- **Payoffs:** `payoff[1,1]=R=3`, `payoff[0,1]=T=5`, `payoff[1,0]=S=0`,
  `payoff[0,0]=P=1`, satisfying `T>R>P>S` and `2R>T+S`.

## 📁 Repository layout

```
AxelrodTournament/
├── wolfram/
│   ├── axelrod.wl        # core engine: payoffs, strategies, noisy match play, round-robin
│   ├── strategies_ext.wl # finite-state-machine engine + modern full-history strategies
│   ├── tournament.wls    # round-robin (Axelrod 1980 reproduction)
│   ├── evolution.wls     # replicator + Moran dynamics, swept over noise
│   ├── discover.wls      # automated memory-one strategy discovery
│   ├── fsm_discover.wls  # [ext] evolve finite-state machines (GP)
│   ├── spatial.wls       # [ext] spatial / lattice evolution
│   ├── coevolution.wls   # [ext] Red-Queen co-evolution
│   ├── ess.wls           # [ext] ESS invasion analysis
│   └── figures.wls       # regenerate base figures
├── autoresearch/         # [ext] Karpathy-style autoresearch loop ported to IPD
│   ├── program.md        #   research instructions
│   ├── evaluate.wls      #   FIXED evaluation harness (do not edit to win)
│   ├── run_search.wls    #   propose→evaluate→keep driver
│   ├── results.tsv       #   experiment log
│   └── champions/        #   discovered strategies that beat TFT
├── tests/sanity.wls      # correctness checks (payoffs, ZD relations)
├── data/                 # computed CSVs (committed for reproducibility)
├── docs/images/          # generated figures (PNG)
├── community/            # Wolfram Community post: build_notebook.wls -> .nb + .pdf
└── analysis.md           # detailed findings (base project)
```

## ▶️ Reproducing

```bash
wolframscript -file tests/sanity.wls            # correctness checks
wolframscript -file wolfram/tournament.wls      # round-robin ranking
wolframscript -file wolfram/evolution.wls       # replicator + Moran vs noise
wolframscript -file wolfram/discover.wls         # memory-one discovery
wolframscript -file autoresearch/run_search.wls  # autoresearch champions
wolframscript -file wolfram/fsm_discover.wls     # evolve finite-state machines
wolframscript -file wolfram/spatial.wls          # spatial / lattice evolution
wolframscript -file wolfram/coevolution.wls      # Red-Queen co-evolution
wolframscript -file wolfram/ess.wls              # ESS invasion analysis
wolframscript -file wolfram/figures.wls          # regenerate base figures
wolframscript -file community/build_notebook.wls # build the Community post (.nb + .pdf)
```

Requires `wolframscript` (Wolfram Language ≥ 14). No external data or API keys.

## 📊 Status — all complete

- [x] Core engine (strategies, payoffs, noisy match play) — verified
- [x] Round-robin tournament (GenerousTFT wins at ε=0; field compresses with noise)
- [x] Evolutionary tournament (replicator + Moran) — **stable mix depends on noise**
- [x] Automated memory-one strategy discovery (beats TFT at every noise level)
- [x] **Ext 1** — Autoresearch champions (7 beat TFT; best Tit-for-2-Tats; novel AdaptiveGenerous)
- [x] **Ext 2** — FSM / genetic-programming discovery
- [x] **Ext 3** — Spatial evolution (noise becomes *corrosive*, not catastrophic)
- [x] **Ext 4** — Red-Queen co-evolution
- [x] **Ext 5** — ESS invasion analysis
- [x] **Wolfram Community post** — `community/axelrod_tournament.nb` + `.pdf` (10 sections, ~18 figures)

### Headline findings

- **Classic tournament (ε=0):** Generous Tit-for-Tat wins; the *extortioner finishes last*. Plain TFT is excellent but edged out by more forgiving cousins.
- **Evolution:** at ε≈0 a coexistence of cooperative strategies; forgiving reciprocators dominate at intermediate noise; at high noise **cooperation collapses** in the well-mixed model. The stable mix is a *function of noise*.
- **Discovery + autoresearch:** propose→evaluate→accept loops find strategies that beat Tit-for-Tat at every noise level; optimal *forgiveness* rises with noise, and must be **patient** (two-strikes), not reflexive.
- **Spatial structure flips the verdict:** in a well-mixed population noise destroys cooperation, but on a lattice clustering keeps it resilient — the *same* noise is catastrophic in one world and merely corrosive in another.
- **"Stable" has three meanings:** ESS (uninvadable → favours defectors), replicator-stable-from-cooperative-start (generous reciprocity → collapse), and spatially-resilient (reciprocator clusters) give **three different, noise-dependent answers**.

---

*An exploration in the Wolfram Language. The discovery and autoresearch loops
borrow the **propose → evaluate → accept → repeat** shape of automated-research
scaffolds (cf. Karpathy's "autoresearch"), re-implemented natively in WL.*
