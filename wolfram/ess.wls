#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  ess.wls -- Extension 5: evolutionary-stability (ESS) invasion analysis.

    For a panel of strategies (classics + modern + the discovered champions),
    and at each noise level, we build the mean-payoff matrix A and ask, for
    every ordered pair (resident R, mutant M): can a rare mutant M invade a
    population of residents R?

        invade if  A[M,R] > A[R,R]   (weak-invasion criterion)
                   or (A[M,R] == A[R,R] and A[M,M] > A[R,M])  (tie-break)

    A strategy is an ESS at this noise level if NO mutant can invade it.

    Outputs: data/ess_*.csv, docs/images/ess_*.png                           *)

Get[FileNameJoin[{DirectoryName[$InputFileName], "axelrod.wl"}]];
Get[FileNameJoin[{DirectoryName[$InputFileName], "strategies_ext.wl"}]];
repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
dataDir  = FileNameJoin[{repoRoot, "data"}];
imgDir   = FileNameJoin[{repoRoot, "docs", "images"}];
Scan[If[! DirectoryQ[#], CreateDirectory[#, CreateIntermediateDirectories -> True]] &,
  {dataDir, imgDir}];

SeedRandom[42];

panel = <||>;
Do[panel[nm] = Axelrod`strategies[nm], {nm, Axelrod`strategyNames}];
Do[panel[nm] = StrategiesExt`extendedStrategies[nm],
  {nm, Keys[StrategiesExt`extendedStrategies]}];
(* Load each champion into its OWN fresh symbol. The champion files all
   define the global `candidate`; capturing that symbol directly would make
   every entry alias the last-loaded (and then-cleared) definition. Instead we
   transfer its DownValues onto a unique symbol. *)
champDir = FileNameJoin[{repoRoot, "autoresearch", "champions"}];
If[DirectoryQ[champDir],
  Do[Module[{nm = FileBaseName[f], sym},
      Block[{candidate},
        Get[f];
        sym = Unique["champ$"];
        DownValues[sym] = DownValues[candidate] /. candidate -> sym;
      ];
      panel["*" <> nm] = sym],
    {f, FileNames["*.wl", champDir]}]];
names = Keys[panel];
S = Values[panel];
n = Length[names];
Print["[ess] panel of ", n, " strategies"];

rounds = 150; reps = 12;
sweep = {0., 0.02, 0.05, 0.1, 0.15, 0.2};

payoffMatrix[eps_] := Module[{A},
  A = Table[0., {n}, {n}];
  Do[
    With[{ab = Axelrod`meanMatch[S[[i]], S[[j]], rounds, eps, reps]},
      A[[i, j]] = ab[[1]]; A[[j, i]] = ab[[2]]],
    {i, n}, {j, i, n}];
  A];

canInvade[A_, m_, r_] := A[[m, r]] > A[[r, r]] + 10.^-6 ||
   (Abs[A[[m, r]] - A[[r, r]]] <= 10.^-6 && A[[m, m]] > A[[r, m]] + 10.^-6);
isStable[A_, r_] := AllTrue[Complement[Range[n], {r}], ! canInvade[A, #, r] &];

Print["[ess] building payoff matrices + invasibility ..."];
stableByNoise = <||>; invMats = <||>;
Do[
  Module[{A, inv, stable, t0 = AbsoluteTime[]},
    A = payoffMatrix[eps];
    inv = Table[Boole[i != j && canInvade[A, i, j]], {i, n}, {j, n}];
    stable = Select[Range[n], isStable[A, #] &];
    invMats[eps] = inv;
    stableByNoise[eps] = names[[stable]];
    Print["   eps=", NumberForm[eps, {4, 3}], "  stable: ",
      If[stable === {}, "(none - cyclic)", names[[stable]]],
      "  (", Round[AbsoluteTime[] - t0, 0.1], "s)"];
  ],
  {eps, sweep}];

Export[FileNameJoin[{dataDir, "ess_stable_vs_noise.csv"}],
  Join[{{"noise", "stableStrategies"}},
    Table[{eps, StringRiffle[stableByNoise[eps], "; "]}, {eps, sweep}]]];
Export[FileNameJoin[{dataDir, "ess_invasion_counts.csv"}],
  Join[{Prepend[ToString /@ sweep, "strategy"]},
    Table[Prepend[Table[Total[invMats[eps][[i]]], {eps, sweep}], names[[i]]],
      {i, n}]]];

heatmap[eps_] := MatrixPlot[invMats[eps],
  ColorRules -> {0 -> Lighter[Gray, 0.7], 1 -> RGBColor[0.78, 0.18, 0.18]},
  FrameTicks -> {
    {Table[{i, names[[i]]}, {i, n}], None},
    {None, Table[{j, Rotate[names[[j]], 90 Degree]}, {j, n}]}},
  PlotLabel -> "Invasibility (row invades column), \[Epsilon]=" <> ToString[eps],
  ImageSize -> 620];
Export[FileNameJoin[{imgDir, "ess_invasibility_lownoise.png"}], heatmap[0.],
  ImageResolution -> 144];
Export[FileNameJoin[{imgDir, "ess_invasibility_highnoise.png"}], heatmap[0.2],
  ImageResolution -> 144];

stableGrid = Table[
  Boole[MemberQ[stableByNoise[eps], names[[i]]]], {i, n}, {eps, sweep}];
Export[FileNameJoin[{imgDir, "ess_stability_map.png"}],
  MatrixPlot[stableGrid,
    ColorRules -> {0 -> White, 1 -> RGBColor[0.18, 0.5, 0.2]},
    FrameTicks -> {
      {Table[{i, names[[i]]}, {i, n}], None},
      {None, Table[{j, sweep[[j]]}, {j, Length[sweep]}]}},
    Mesh -> All, FrameLabel -> {"", "execution noise \[Epsilon]"},
    PlotLabel -> "Evolutionarily stable strategies vs noise (green = no mutant invades)",
    ImageSize -> 620],
  ImageResolution -> 144];

Print["[ess] done. stable strategies by noise:"];
Do[Print["   eps=", eps, ": ", stableByNoise[eps]], {eps, sweep}];
