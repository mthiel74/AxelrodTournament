#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  evolution.wls -- Phase 3: evolutionary tournament.

    Two dynamics on the strategy field, both driven by the round-robin
    mean-payoff matrix A (A[[i,j]] = mean score of i vs j):

      * Replicator dynamics (deterministic) -- headline result.
        x_i(t+1) = x_i(t) * f_i / phi,   f = A.x,  phi = x.A.x.
        A small mutation term keeps the dynamics interior so a unique
        "stable mix" exists.

      * Moran process (stochastic, finite population) -- robustness.
        Frequency-dependent reproduction with rare mutation; we report
        the long-run average composition.

    We sweep the noise level eps and show the stable mix depends on it.
    Outputs: data/*.csv and docs/images/*.png                          *)

Get[FileNameJoin[{DirectoryName[$InputFileName], "axelrod.wl"}]];
repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
dataDir  = FileNameJoin[{repoRoot, "data"}];
imgDir   = FileNameJoin[{repoRoot, "docs", "images"}];
Scan[If[! DirectoryQ[#], CreateDirectory[#, CreateIntermediateDirectories -> True]] &,
  {dataDir, imgDir}];

SeedRandom[2026];
names = Axelrod`strategyNames;
n     = Length[names];

(* ---- parameters ---------------------------------------------------- *)
rounds   = 150;
reps     = 60;
mu       = 0.001;                 (* replicator mutation (keeps interior) *)
genSteps = 4000;                  (* replicator iterations *)
sweep    = Range[0., 0.20, 0.02]; (* noise levels for the headline sweep *)
popN     = 100;                   (* Moran population size *)
moranSteps = 200000;              (* Moran steps *)
moranMu  = 0.002;                 (* Moran mutation rate *)

(* ---- payoff matrices A[eps] (cache) -------------------------------- *)
Print["[evolution] building payoff matrices for ", Length[sweep], " noise levels..."];
Amats = Association @ Table[
  Module[{t0 = AbsoluteTime[], A},
    A = Axelrod`tournamentMatrix[names, rounds, eps, reps];
    Print["   eps=", NumberForm[eps, {4, 3}], "  (",
      Round[AbsoluteTime[] - t0, 0.1], "s)"];
    eps -> A],
  {eps, sweep}];

(* ---- replicator (with mutation) ----------------------------------- *)
replicator[A_, steps_, mut_] := Module[{x, f, phi, traj},
  x = ConstantArray[1./n, n];
  traj = Table[
    f   = A . x;
    phi = x . f;
    x   = x*f/phi;                         (* selection *)
    x   = (1 - mut) x + mut/n;             (* mutation toward uniform *)
    x   = x/Total[x];
    x,
    {steps}];
  <|"final" -> Last[traj], "traj" -> traj|>
];

Print["[evolution] running replicator dynamics..."];
repFinal = Association @ Table[
  eps -> replicator[Amats[eps], genSteps, mu]["final"], {eps, sweep}];

(* representative trajectories at low / high noise *)
trajLow  = replicator[Amats[0.], genSteps, mu]["traj"];
trajHigh = replicator[Amats[0.2], genSteps, mu]["traj"];

(* ---- Moran process ------------------------------------------------- *)
(* state = counts vector summing to popN; payoff to a strategy = mean
   score vs the rest of the population (self excluded).               *)
moran[A_, N0_, steps_, mut_] := Module[
  {counts, fitness, avg, total, birth, death, cum, r, i, j, k},
  counts = Floor[N0/n] ConstantArray[1, n];
  counts[[1]] += N0 - Total[counts];           (* make it sum to N0 *)
  avg = ConstantArray[0., n];
  Do[
    (* frequency-dependent fitness (exclude self interaction) *)
    fitness = Table[
      If[counts[[i]] == 0, 0.,
        (A[[i]] . counts - A[[i, i]])/(N0 - 1)], {i, n}];
    fitness = fitness*counts;                   (* weight by abundance *)
    total = Total[fitness];
    (* choose birth strategy ~ fitness; with prob mut, uniform mutant *)
    If[RandomReal[] < mut,
      birth = RandomInteger[{1, n}],
      r = RandomReal[] total; cum = 0.; birth = n;
      Do[cum += fitness[[k]]; If[cum >= r, birth = k; Break[]], {k, n}]];
    (* choose death ~ abundance *)
    r = RandomInteger[{1, N0}]; cum = 0; death = n;
    Do[cum += counts[[k]]; If[cum >= r, death = k; Break[]], {k, n}];
    counts[[birth]]++; counts[[death]]--;
    If[t > steps/5, avg += counts],             (* average after burn-in *)
    {t, steps}];
  avg/Total[avg]
];

Print["[evolution] running Moran process (", moranSteps, " steps each)..."];
moranFinal = Association @ Table[
  Module[{t0 = AbsoluteTime[], m},
    m = moran[Amats[eps], popN, moranSteps, moranMu];
    Print["   Moran eps=", NumberForm[eps, {4, 3}], "  (",
      Round[AbsoluteTime[] - t0, 0.1], "s)"];
    eps -> m],
  {eps, sweep}];

(* ---- save numeric results ----------------------------------------- *)
exportComposition[file_, assoc_] := Export[
  FileNameJoin[{dataDir, file}],
  Join[{Prepend[names, "noise"]},
    Table[Prepend[assoc[eps], eps], {eps, sweep}]]];
exportComposition["replicator_vs_noise.csv", repFinal];
exportComposition["moran_vs_noise.csv", moranFinal];

(* ---- figures ------------------------------------------------------- *)
palette = ColorData[97, "ColorList"][[;; n]];

stackPlot[assoc_, title_] := Module[{rows},
  rows = Table[assoc[eps], {eps, sweep}];
  StackedListPlot[Transpose[rows],
    PlotLayout -> "Stacked",
    DataRange -> {First[sweep], Last[sweep]},
    PlotStyle -> palette,
    PlotLegends -> Placed[LineLegend[palette, names], Right],
    PlotRange -> {0, 1}, Frame -> True,
    FrameLabel -> {"execution noise \[Epsilon]", "population fraction"},
    PlotLabel -> title, ImageSize -> 720, AspectRatio -> 0.55]];

Export[FileNameJoin[{imgDir, "replicator_vs_noise.png"}],
  stackPlot[repFinal, "Evolutionarily stable mix vs noise (replicator)"],
  ImageResolution -> 144];
Export[FileNameJoin[{imgDir, "moran_vs_noise.png"}],
  stackPlot[moranFinal, "Long-run composition vs noise (Moran, N=" <> ToString[popN] <> ")"],
  ImageResolution -> 144];

trajPlot[traj_, title_] := StackedListPlot[Transpose[traj],
  PlotLayout -> "Stacked", PlotStyle -> palette,
  PlotLegends -> Placed[LineLegend[palette, names], Right],
  PlotRange -> {0, 1}, Frame -> True,
  FrameLabel -> {"generation", "population fraction"},
  PlotLabel -> title, ImageSize -> 720, AspectRatio -> 0.5];

Export[FileNameJoin[{imgDir, "replicator_traj_lownoise.png"}],
  trajPlot[trajLow, "Replicator trajectory, \[Epsilon]=0"], ImageResolution -> 144];
Export[FileNameJoin[{imgDir, "replicator_traj_highnoise.png"}],
  trajPlot[trajHigh, "Replicator trajectory, \[Epsilon]=0.2"], ImageResolution -> 144];

Print["[evolution] done. Stable mix at eps=0 vs eps=0.2:"];
Print["  eps=0  : ", Sort[Thread[names -> Round[repFinal[0.], 0.01]], #1[[2]] > #2[[2]] &][[;; 5]]];
Print["  eps=0.2: ", Sort[Thread[names -> Round[repFinal[0.2], 0.01]], #1[[2]] > #2[[2]] &][[;; 5]]];
