# Project conventions — AxelrodTournament

Iterated Prisoner's Dilemma in the Wolfram Language: reproduce Axelrod 1980, run
an evolutionary tournament vs noise, an automated strategy-discovery loop, and
five advanced extensions (autoresearch champions, FSM/GP, spatial, co-evolution,
ESS). Deliverable: a Wolfram Community post (a programmatically-built `.nb` +
`.pdf`).

## Layout & conventions (mirrors the ENSO-emergence project)

- `wolfram/*.wls` — executable entry points. Header:
  ```
  #!/usr/bin/env wolframscript
  (* ::Package:: *)
  ```
  Load the core package with
  `Get[FileNameJoin[{DirectoryName[$InputFileName], "axelrod.wl"}]]` and derive
  paths from `repoRoot = ParentDirectory @ DirectoryName[$InputFileName]`.
  `CreateDirectory` outputs if missing. Use `Print["[stage] …"]` logging.
- `wolfram/*.wl` — packages loaded via `Get` (engine `axelrod.wl`; extended
  strategies + FSM engine `strategies_ext.wl`).
- `data/` — tidy outputs committed for reproducibility (CSV; `*.mx`/`*.m`
  git-ignored as regenerable binaries).
- `docs/images/` — figures, `Export[..., ImageResolution -> 144]`.
- `community/` — `build_notebook.wls` builds the post from `Cell[]` expressions
  (helpers in `post_helpers.wl`) and exports `.nb` + `.pdf`.
- `autoresearch/` — the Karpathy-style loop: `evaluate.wls` is the FIXED harness
  (never edit it to win); `run_search.wls` proposes/evaluates/keeps; champions
  are saved to `champions/*.wl`.
- `tests/` — sanity checks (payoffs, ZD enforced relations).

## Model invariants

- Move encoding **C = 1, D = 0**. Payoffs **T=5 > R=3 > P=1 > S=0**, `2R > T+S`.
- A strategy is `s[myHist, oppHist] -> 0|1`; histories are chronological actual
  moves, empty on opening.
- Noise is **execution error**: flip each intended move w.p. `ε`; opponents see
  the actual move.

## WL gotchas (learned the hard way)

- When ranking, apply `N[]` to scores before `Ordering`/`Sort`.
- `NumberForm[...]` objects do **not** serialise to CSV/TSV — write plain numbers
  (`N@Round[x, 0.0001]`).
- `Export[file, expr, ".m"]` can choke on associations with real-valued keys —
  use `Put[expr, file]`.
- Only run one `wolframscript` at a time when building notebooks (a second kernel
  contends for the single front-end license → `FrontEndObject::notavail`).

## Workflow

- Always run scripts and check output before reporting done.
- Commit after each meaningful stage; repo is **private**; push regularly.
- After any tooling hiccup, re-verify `git rev-parse HEAD origin/main` — don't
  trust a "pushed" message that may have been cancelled.
