#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  spatial.wls -- Extension 3: spatial / networked evolution of cooperation.

    Strategies live on the sites of an L x L lattice (torus, von-Neumann
    neighbourhood). Each generation:
      1. every site plays the IPD against its 4 neighbours, accumulating
         fitness = total mean-match score;
      2. each site picks a random neighbour and adopts its strategy with the
         Fermi probability  P = 1/(1 + exp(-(f_nb - f_self)/(K*scale))),
         i.e. it tends to imitate fitter neighbours (Szabo-Toke dynamics);
      3. tiny mutation keeps the dynamics ergodic.

    Nowak & May's classic result: spatial structure lets cooperators survive
    by clustering even when they'd vanish in a well-mixed population. We ask
    how this depends on execution NOISE -- and find the striking contrast to
    our well-mixed replicator result: under noise the lattice does NOT
    collapse to defection; instead the error-correcting Pavlov (WSLS) takes
    over, sustaining cooperation.

    Outputs: data/spatial_vs_noise.csv, docs/images/spatial_*.png            *)

Get[FileNameJoin[{DirectoryName[$InputFileName], "axelrod.wl"}]];
repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
dataDir  = FileNameJoin[{repoRoot, "data"}];
imgDir   = FileNameJoin[{repoRoot, "docs", "images"}];
Scan[If[! DirectoryQ[#], CreateDirectory[#, CreateIntermediateDirectories -> True]] &,
  {dataDir, imgDir}];

SeedRandom[2718];

paletteNames = {"AllC", "AllD", "TitForTat", "GenerousTFT", "Pavlov", "Grim"};
strat = Axelrod`strategies /@ paletteNames;
nS = Length[paletteNames];
cols = Table[ColorData["Rainbow"][(i - 1)/(nS - 1)], {i, nS}];

L = 32;                 (* lattice side *)
gens = 80;              (* generations *)
matchRounds = 40;
matchReps = 5;
Ksel = 0.5;             (* Fermi selection intensity (lower = greedier) *)
Tmax = 5;               (* max single-round payoff, for normalising fitness diffs *)
mutRate = 0.01;
sweep = {0., 0.02, 0.05, 0.1, 0.15, 0.2};

(* mean per-match score table between strategy types at noise eps *)
scoreTable[eps_] := Table[
  Axelrod`meanMatch[strat[[i]], strat[[j]], matchRounds, eps, matchReps][[1]],
  {i, nS}, {j, nS}];

neighbors[{r_, c_}] := {{Mod[r, L] + 1, c}, {Mod[r - 2, L] + 1, c},
   {r, Mod[c, L] + 1}, {r, Mod[c - 2, L] + 1}};

runLattice[eps_] := Module[{T, grid, fit, snaps = {}},
  T = scoreTable[eps];
  grid = RandomInteger[{1, nS}, {L, L}];
  Do[
    fit = Table[
      Total[T[[grid[[r, c]], grid[[#[[1]], #[[2]]]]]] & /@ neighbors[{r, c}]],
      {r, L}, {c, L}];
    grid = Table[
      Module[{nb = RandomChoice[neighbors[{r, c}]], fi, fj, p},
        fi = fit[[r, c]]; fj = fit[[nb[[1]], nb[[2]]]];
        p = 1/(1 + Exp[-(fj - fi)/(Ksel*4*Tmax)]);
        If[RandomReal[] < mutRate, RandomInteger[{1, nS}],
          If[RandomReal[] < p, grid[[nb[[1]], nb[[2]]]], grid[[r, c]]]]],
      {r, L}, {c, L}];
    If[g == 1 || g == Round[gens/2] || g == gens, AppendTo[snaps, grid]],
    {g, gens}];
  {grid, snaps}
];

Print["[spatial] ", L, "x", L, " lattice, ", gens, " gens, Fermi imitation; palette ", paletteNames];
finalComp = <||>; snapsAt = <||>;
Do[
  Module[{t0 = AbsoluteTime[], res, grid, comp},
    res = runLattice[eps]; grid = res[[1]]; snapsAt[eps] = res[[2]];
    comp = Table[N@Lookup[Counts[Flatten[grid]], i, 0]/L^2, {i, nS}];
    finalComp[eps] = comp;
    Print["   eps=", NumberForm[eps, {4, 3}], "  comp=", Round[comp, 0.01],
      "  (", Round[AbsoluteTime[] - t0, 0.1], "s)"];
  ],
  {eps, sweep}];

allDidx = Position[paletteNames, "AllD"][[1, 1]];
coopShare[comp_] := 1 - comp[[allDidx]];

Export[FileNameJoin[{dataDir, "spatial_vs_noise.csv"}],
  Join[{Prepend[paletteNames, "noise"]},
    Table[Prepend[finalComp[eps], eps], {eps, sweep}]]];

(* ---- figures ------------------------------------------------------- *)
Export[FileNameJoin[{imgDir, "spatial_composition_vs_noise.png"}],
  Module[{series = Transpose[Table[finalComp[eps], {eps, sweep}]]},
    StackedListPlot[Table[Transpose[{sweep, series[[i]]}], {i, nS}],
      PlotStyle -> cols, Filling -> Automatic,
      PlotRange -> {{0, Last[sweep]}, {0, 1}}, Frame -> True,
      FrameLabel -> {"execution noise \[Epsilon]", "lattice fraction"},
      PlotLabel -> "Spatial evolution: final composition vs noise (Pavlov rescues cooperation)",
      PlotLegends -> SwatchLegend[cols, paletteNames], ImageSize -> 760,
      AspectRatio -> 0.5]],
  ImageResolution -> 144];

latticeImg[grid_, lbl_] := Labeled[
  ArrayPlot[grid, ColorRules -> Thread[Range[nS] -> cols],
    Frame -> False, ImageSize -> 200, Mesh -> False, PlotRangePadding -> 0],
  lbl, Top];
Export[FileNameJoin[{imgDir, "spatial_snapshots.png"}],
  Module[{lo = snapsAt[0.], hi = snapsAt[Last[sweep]]},
    Grid[{
      {Style["\[Epsilon] = 0", Bold], SpanFromLeft, SpanFromLeft},
      {latticeImg[lo[[1]], "gen 1"], latticeImg[lo[[2]], "gen " <> ToString[Round[gens/2]]],
       latticeImg[lo[[3]], "gen " <> ToString[gens]]},
      {Style["\[Epsilon] = " <> ToString[Last[sweep]], Bold], SpanFromLeft, SpanFromLeft},
      {latticeImg[hi[[1]], "gen 1"], latticeImg[hi[[2]], "gen " <> ToString[Round[gens/2]]],
       latticeImg[hi[[3]], "gen " <> ToString[gens]]}},
     Frame -> All, Spacings -> {1.5, 1.5},
     Dividers -> {{False}, {False}}]],
  ImageResolution -> 144];

(* legend for the snapshot grid *)
Export[FileNameJoin[{imgDir, "spatial_legend.png"}],
  SwatchLegend[cols, paletteNames, LegendLayout -> "Row"], ImageResolution -> 144];

Print["[spatial] cooperation share by noise: ",
  Table[eps -> Round[coopShare[finalComp[eps]], 0.01], {eps, sweep}]];
Print["[spatial] done. Contrast with well-mixed: spatial structure + Pavlov ",
  "sustains cooperation under noise rather than collapsing to defection."];
