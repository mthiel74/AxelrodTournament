#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  fsm_discover.wls -- Extension 1: evolve finite-state-machine strategies
    by genetic programming, beyond the memory-one box.

    For each noise level we run a GA over k-state Mealy machines, scoring
    each machine by its mean field score against the standard field. We
    record the best machine, its fitness vs Tit-for-Tat, and its number of
    reachable states (a complexity measure). The headline: discovered
    machines grow MORE internal structure as noise rises -- they evolve
    error-correcting / forgiving sub-automata that memory-one cannot express.

    Outputs: data/fsm_discovered.csv, data/fsm_best_genes.m, and
    docs/images figures (rendered here directly; small + self-contained).  *)

Get[FileNameJoin[{DirectoryName[$InputFileName], "axelrod.wl"}]];
Get[FileNameJoin[{DirectoryName[$InputFileName], "strategies_ext.wl"}]];
repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
dataDir  = FileNameJoin[{repoRoot, "data"}];
imgDir   = FileNameJoin[{repoRoot, "docs", "images"}];
Scan[If[! DirectoryQ[#], CreateDirectory[#, CreateIntermediateDirectories -> True]] &,
  {dataDir, imgDir}];

SeedRandom[314];
names = Axelrod`strategyNames;
field = Axelrod`strategies /@ names;
nField = Length[field];

(* ---- parameters ---------------------------------------------------- *)
kStates  = 4;             (* machine size *)
rounds   = 120;
reps     = 6;
popSize  = 40;
gens     = 22;
mutRate  = 0.10;
evalSeed = 9001;
sweep    = {0., 0.02, 0.05, 0.1, 0.15, 0.2};

arity = StrategiesExt`fsmArity[kStates];

(* deterministic fitness: mean field score of a machine at noise eps *)
fitnessGenes[genes_, eps_] := BlockRandom[
  SeedRandom[evalSeed];
  Module[{s = StrategiesExt`fsmFromGenes[kStates, genes]},
    Mean @ Table[Axelrod`meanMatch[s, opp, rounds, eps, reps][[1]], {opp, field}]]
];

tftScore[eps_] := BlockRandom[SeedRandom[evalSeed];
  Mean @ Table[Axelrod`meanMatch[Axelrod`strategies["TitForTat"], opp, rounds, eps, reps][[1]], {opp, field}]];

(* ---- GA over FSM genomes ------------------------------------------ *)
evolveFSM[eps_] := Module[
  {pop, fits, ranked, elite, nElite, children, best, bestF, hist = {}},
  pop = Table[StrategiesExt`randomFSM[kStates], {popSize}];
  (* seed one TFT-like machine so we never do worse than TFT *)
  pop[[1]] = Join[{1}, Flatten@Table[{1, 1, 0, 1}, {kStates}]];
  nElite = Max[2, Round[popSize/5]];
  Do[
    fits = fitnessGenes[#, eps] & /@ pop;
    ranked = Reverse@Ordering[N@fits];
    pop = pop[[ranked]]; fits = fits[[ranked]];
    best = pop[[1]]; bestF = fits[[1]]; AppendTo[hist, bestF];
    elite = pop[[;; nElite]];
    children = Table[
      StrategiesExt`mutateFSM[kStates,
        StrategiesExt`crossoverFSM @@ RandomChoice[elite, 2], mutRate],
      {popSize - nElite}];
    pop = Join[elite, children],
    {gens}];
  <|"genes" -> best, "fit" -> bestF, "hist" -> hist,
    "complexity" -> StrategiesExt`fsmComplexity[kStates, best]|>
];

Print["[fsm] evolving ", kStates, "-state machines over noise ", sweep, " ..."];
results = Association @ Table[
  Module[{t0 = AbsoluteTime[], r},
    r = evolveFSM[eps];
    Print["   eps=", NumberForm[eps, {4, 3}],
      "  fit=", NumberForm[r["fit"], {5, 3}],
      "  TFT=", NumberForm[tftScore[eps], {5, 3}],
      "  states=", r["complexity"],
      "  (", Round[AbsoluteTime[] - t0, 0.1], "s)"];
    eps -> r],
  {eps, sweep}];

(* ---- save ---------------------------------------------------------- *)
Export[FileNameJoin[{dataDir, "fsm_discovered.csv"}],
  Join[{{"noise", "fsmFieldScore", "tftFieldScore", "improvement", "reachableStates"}},
    Table[{eps, results[eps]["fit"], tftScore[eps],
           results[eps]["fit"] - tftScore[eps], results[eps]["complexity"]},
      {eps, sweep}]]];
Export[FileNameJoin[{dataDir, "fsm_best_genes.m"}],
  <|"kStates" -> kStates, "sweep" -> sweep,
    "genes" -> Association@Table[eps -> results[eps]["genes"], {eps, sweep}]|>];

(* ---- figures ------------------------------------------------------- *)
Export[FileNameJoin[{imgDir, "fsm_vs_tft.png"}],
  ListLinePlot[{
     Table[{eps, results[eps]["fit"]}, {eps, sweep}],
     Table[{eps, tftScore[eps]}, {eps, sweep}]},
    PlotStyle -> {Directive[Thick, ColorData[97][1]],
                  Directive[Thick, Dashed, ColorData[97][4]]},
    PlotMarkers -> Automatic, Frame -> True,
    FrameLabel -> {"execution noise \[Epsilon]", "mean field score"},
    PlotLabel -> "Evolved finite-state machines vs Tit-for-Tat",
    PlotLegends -> {"evolved FSM", "Tit-for-Tat"}, ImageSize -> 700],
  ImageResolution -> 144];

Export[FileNameJoin[{imgDir, "fsm_complexity_vs_noise.png"}],
  ListLinePlot[Table[{eps, results[eps]["complexity"]}, {eps, sweep}],
    PlotStyle -> Directive[Thick, ColorData[97][3]], PlotMarkers -> Automatic,
    Frame -> True, PlotRange -> {0, kStates + 0.5},
    FrameLabel -> {"execution noise \[Epsilon]", "reachable states of best machine"},
    PlotLabel -> "Evolved strategy complexity grows with noise", ImageSize -> 700],
  ImageResolution -> 144];

Print["[fsm] done. complexity by noise: ",
  Table[eps -> results[eps]["complexity"], {eps, sweep}]];
