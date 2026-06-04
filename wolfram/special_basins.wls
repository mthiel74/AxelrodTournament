#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  special_basins.wls -- basins of attraction of the replicator dynamics, a
    phase-portrait view of "where does cooperation win?" that IPD intros rarely
    show.

    We take a 3-strategy world {AllD, TitForTat, GenerousTFT}; the state is a
    point on the 2-simplex (mixtures summing to 1). From a fine grid of initial
    mixtures we run replicator dynamics to convergence and colour each starting
    point by which vertex/attractor it ends at. The result is a basin map on the
    simplex. Sweeping execution noise shows the cooperative basin shrink as
    noise rises -- an animation -- and at the end the AllD basin swallows the
    triangle.

    Outputs: docs/images/basins_vs_noise.gif, docs/images/basins_grid.png
             data/basins_*.m                                                   *)

Get[FileNameJoin[{DirectoryName[$InputFileName], "axelrod.wl"}]];
repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
dataDir  = FileNameJoin[{repoRoot, "data"}];
imgDir   = FileNameJoin[{repoRoot, "docs", "images"}];
Scan[If[! DirectoryQ[#], CreateDirectory[#, CreateIntermediateDirectories -> True]] &,
  {dataDir, imgDir}];

SeedRandom[1234];

triNames = {"AllD", "TitForTat", "GenerousTFT"};
triStrat = Axelrod`strategies /@ triNames;
nT = 3;
triCols = {RGBColor[0.75, 0.2, 0.2], RGBColor[0.2, 0.4, 0.75], RGBColor[0.2, 0.6, 0.3]};

rounds = 200; reps = 30;

(* payoff matrix among the three, at noise eps *)
payoffMat[eps_] := Table[
  Axelrod`meanMatch[triStrat[[i]], triStrat[[j]], rounds, eps, reps][[1]],
  {i, nT}, {j, nT}];

(* replicator to convergence; return index of dominant strategy at the end *)
attractor[A_, x0_] := Module[{x = x0, f, phi},
  Do[f = A . x; phi = x . f; x = x f/phi; x = x/Total[x];
     If[Max[x] > 0.999, Break[]], {2500}];
  First@Ordering[-x]];

(* barycentric -> cartesian for plotting the simplex *)
v1 = {0, 0}; v2 = {1, 0}; v3 = {0.5, Sqrt[3]/2};
bary[{a_, b_, c_}] := a v1 + b v2 + c v3;

res = 70;   (* grid resolution across the simplex *)
basinPoints[A_] := Reap[
  Do[
    With[{a = i/res, b = j/res},
     If[a + b <= 1,
      Sow[{bary[{a, b, 1 - a - b}], attractor[A, {a, b, 1 - a - b}]}]]],
    {i, 0, res}, {j, 0, res}]][[2, 1]];

basinPlot[pts_, eps_] := Graphics[{
   PointSize[0.012],
   {triCols[[#[[2]]]], Point[#[[1]]]} & /@ pts,
   Black, Thick, Line[{v1, v2, v3, v1}],
   Text[Style[triNames[[1]], 11, triCols[[1]]], v1 - {0.04, 0.03}],
   Text[Style[triNames[[2]], 11, triCols[[2]]], v2 + {0.04, -0.03}],
   Text[Style[triNames[[3]], 11, triCols[[3]]], v3 + {0, 0.04}]},
  PlotLabel -> Style["Replicator basins of attraction, \[Epsilon]=" <> ToString[eps], 13],
  ImageSize -> 360, PlotRange -> {{-0.12, 1.12}, {-0.1, 1.0}}];

noiseFrames = {0., 0.03, 0.06, 0.1, 0.15, 0.2};
Print["[basins] computing basins over ", Length[noiseFrames], " noise levels (res=", res, ") ..."];
basinsAt = Table[
  Module[{t0 = AbsoluteTime[], A = payoffMat[eps], pts},
    pts = basinPoints[A];
    Print["   eps=", NumberForm[eps, {4, 3}], "  coop-basin frac=",
      NumberForm[N@Mean[Boole[#[[2]] != 1] & /@ pts], {4, 3}],
      "  (", Round[AbsoluteTime[] - t0, 0.1], "s)"];
    eps -> pts],
  {eps, noiseFrames}];

Put[<|"noise" -> noiseFrames, "v" -> {v1, v2, v3}, "names" -> triNames,
   "points" -> Association[basinsAt]|>, FileNameJoin[{dataDir, "basins.m"}]];

Export[FileNameJoin[{imgDir, "basins_vs_noise.gif"}],
  Table[basinPlot[(eps /. basinsAt), eps], {eps, noiseFrames}],
  "GIF", "DisplayDurations" -> 0.8, AnimationRepetitions -> Infinity,
  ImageResolution -> 100];

Export[FileNameJoin[{imgDir, "basins_grid.png"}],
  Grid[Partition[Table[basinPlot[(eps /. basinsAt), eps], {eps, noiseFrames}], 3],
    Spacings -> {0.5, 0.5}],
  ImageResolution -> 120];

Print["[basins] done -> basins_vs_noise.gif, basins_grid.png"];
