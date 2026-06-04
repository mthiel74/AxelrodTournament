#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  special_fingerprints.wls -- strategy "fingerprints" (Ashlock 2008), a
    technique almost never seen in introductory IPD treatments.

    A fingerprint summarises a strategy S as a 2D image. We play S against a
    one-parameter family of probabilistic opponents -- the "Joss-Ann" transform
    of Tit-for-Tat: with prob x act as AllD, with prob y act as AllC, otherwise
    play TFT. Sweeping (x,y) in the unit square and recording S's mean score
    gives a unique surface -- a fingerprint that identifies the strategy and
    exposes how exploitable / forgiving it is.

    Novel twist (ours): we ANIMATE the fingerprint as execution noise rises,
    showing how a strategy's "character" smears as the channel degrades. We
    fingerprint Tit-for-Tat, Grim, Pavlov and the discovered memory-two
    champion.

    Outputs: docs/images/fingerprints_grid.png (the four strategies, eps=0),
             docs/images/fingerprint_champion_noise.gif (champion vs noise),
             data/fingerprint_*.m (committed surfaces).                        *)

Get[FileNameJoin[{DirectoryName[$InputFileName], "axelrod.wl"}]];
repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
dataDir  = FileNameJoin[{repoRoot, "data"}];
imgDir   = FileNameJoin[{repoRoot, "docs", "images"}];
Scan[If[! DirectoryQ[#], CreateDirectory[#, CreateIntermediateDirectories -> True]] &,
  {dataDir, imgDir}];

SeedRandom[909];

(* load the champion robustly: Get defines global `candidate`; copy its
   DownValues onto our own symbol `memTwo` (Block-scoping it would clear the
   definition before memTwo is ever called -> symbolic candidate[] -> crash). *)
champFile = FileNameJoin[{repoRoot, "autoresearch", "champions", "MemTwo-Long.wl"}];
Get[champFile];
DownValues[memTwo] = DownValues[candidate] /. candidate -> memTwo;
Clear[candidate];

(* Joss-Ann(TFT) probe opponent: prob x -> defect, prob y -> cooperate,
   else play TFT (copy opp's last move). Probabilities sum-constrained x+y<=1. *)
probe[x_, y_] := Function[{me, opp},
  Module[{u = RandomReal[]},
    Which[u < x, 0, u < x + y, 1,
      opp === {}, 1, True, Last[opp]]]];

rounds = 120; reps = 6; grid = 16;   (* grid x grid resolution *)

fingerprint[strategy_, eps_] := Table[
  If[x + y > 1, Missing[],   (* outside the valid simplex *)
    Axelrod`meanMatch[strategy, probe[x, y], rounds, eps, reps][[1]]],
  {y, 0, 1, 1/(grid - 1)}, {x, 0, 1, 1/(grid - 1)}];

fpPlot[surf_, label_] := ArrayPlot[
  (surf /. {_Missing -> 0, Null -> 0}),   (* invalid simplex cells -> 0 *)
  DataReversed -> True,                    (* y increases upward *)
  ColorFunction -> "SunsetColors", ColorFunctionScaling -> False,
  PlotRange -> {0, 5}, Frame -> True,
  FrameTicks -> {{{1, "1"}, {grid, "0"}}, {{1, "0"}, {grid, "1"}}},
  FrameLabel -> {"x  (probe defects)", "y  (probe cooperates)"},
  PlotLabel -> label, ImageSize -> 300];

names = {"TitForTat", "Grim", "Pavlov"};
strats = Append[Axelrod`strategies /@ names, memTwo];
allNames = Append[names, "MemTwo-Long"];

Print["[fingerprint] computing 4 fingerprints at eps=0 ..."];
surf0 = Table[
  Module[{t0 = AbsoluteTime[], s = fingerprint[strats[[k]], 0.]},
    Print["   ", allNames[[k]], "  (", Round[AbsoluteTime[] - t0, 0.1], "s)"]; s],
  {k, Length[strats]}];

Export[FileNameJoin[{imgDir, "fingerprints_grid.png"}],
  Labeled[
    Grid[Partition[MapThread[fpPlot[#1, #2] &, {surf0, allNames}], 2],
      Spacings -> {1, 1}],
    Style["Strategy fingerprints (Ashlock): mean score vs the Joss-Ann(TFT) probe, \[Epsilon]=0",
      14], Top],
  ImageResolution -> 120];
Put[<|"names" -> allNames, "surfaces" -> surf0, "grid" -> grid|>,
  FileNameJoin[{dataDir, "fingerprint_eps0.m"}]];

(* ---- animate the champion's fingerprint as noise rises ------------- *)
noiseFrames = {0., 0.03, 0.06, 0.1, 0.15, 0.2, 0.3};
Print["[fingerprint] animating champion fingerprint over ", Length[noiseFrames], " noise levels ..."];
champSurfs = Table[
  Module[{t0 = AbsoluteTime[], s = fingerprint[memTwo, eps]},
    Print["   eps=", NumberForm[eps, {4, 3}], "  (", Round[AbsoluteTime[] - t0, 0.1], "s)"];
    eps -> s],
  {eps, noiseFrames}];
Put[<|"noise" -> noiseFrames, "surfaces" -> Association[champSurfs], "grid" -> grid|>,
  FileNameJoin[{dataDir, "fingerprint_champion_noise.m"}]];

Export[FileNameJoin[{imgDir, "fingerprint_champion_noise.gif"}],
  Table[fpPlot[(eps /. champSurfs),
     "MemTwo-Long fingerprint, \[Epsilon]=" <> ToString[eps]], {eps, noiseFrames}],
  "GIF", "DisplayDurations" -> 0.7, AnimationRepetitions -> Infinity,
  ImageResolution -> 100];

Print["[fingerprint] done -> fingerprints_grid.png, fingerprint_champion_noise.gif"];
