#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  special_spatial_movie.wls -- "cooperation weather map".

    Animate the spatial IPD on a lattice: every generation is a frame, so you
    watch cooperative clusters nucleate, spread, and defend themselves against
    invading defectors (Nowak-May spatial reciprocity) -- and how that motion
    changes with execution noise. We render two side-by-side worlds at low and
    high noise to make the contrast a single moving image, plus a full-res
    high-noise movie. The discovered memory-two champion (MemTwo-Long) is
    included in the palette as a seventh strategy.

    Outputs: docs/images/spatial_movie.gif, docs/images/spatial_movie_compare.gif
             data/spatial_movie_frames.m (committed grids for reproducibility)  *)

Get[FileNameJoin[{DirectoryName[$InputFileName], "axelrod.wl"}]];
Get[FileNameJoin[{DirectoryName[$InputFileName], "strategies_ext.wl"}]];
repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
dataDir  = FileNameJoin[{repoRoot, "data"}];
imgDir   = FileNameJoin[{repoRoot, "docs", "images"}];
Scan[If[! DirectoryQ[#], CreateDirectory[#, CreateIntermediateDirectories -> True]] &,
  {dataDir, imgDir}];

SeedRandom[2718];

(* load the champion robustly: copy DownValues of the file's `candidate` onto
   our symbol (Block-scoping clears it before use -> crash). *)
champFile = FileNameJoin[{repoRoot, "autoresearch", "champions", "MemTwo-Long.wl"}];
Get[champFile];
DownValues[memTwoChampion] = DownValues[candidate] /. candidate -> memTwoChampion;
Clear[candidate];

paletteNames = {"AllC", "AllD", "TitForTat", "GenerousTFT", "Pavlov", "Grim", "MemTwo"};
strat = Append[Axelrod`strategies /@ Most[paletteNames], memTwoChampion];
nS = Length[paletteNames];
cols = Append[Table[ColorData["Rainbow"][(i - 1)/(nS - 2)], {i, nS - 1}],
   RGBColor[0.85, 0.33, 0.1]];   (* champion in signature orange *)

L = 48; gens = 120; matchRounds = 40; matchReps = 4;
Ksel = 0.5; Tmax = 5; mutRate = 0.008;

scoreTable[eps_] := Table[
  Axelrod`meanMatch[strat[[i]], strat[[j]], matchRounds, eps, matchReps][[1]],
  {i, nS}, {j, nS}];

neighbors[{r_, c_}] := {{Mod[r, L] + 1, c}, {Mod[r - 2, L] + 1, c},
   {r, Mod[c, L] + 1}, {r, Mod[c - 2, L] + 1}};

(* return ALL generation grids (the movie frames) *)
runFrames[eps_, T_] := Module[{grid, fit, frames},
  grid = RandomInteger[{1, nS}, {L, L}];
  frames = {grid};
  Do[
    fit = Table[Total[T[[grid[[r, c]], grid[[#[[1]], #[[2]]]]]] & /@ neighbors[{r, c}]],
      {r, L}, {c, L}];
    grid = Table[
      Module[{nb = RandomChoice[neighbors[{r, c}]], fi, fj, p},
        fi = fit[[r, c]]; fj = fit[[nb[[1]], nb[[2]]]];
        p = 1/(1 + Exp[-(fj - fi)/(Ksel*4*Tmax)]);
        If[RandomReal[] < mutRate, RandomInteger[{1, nS}],
          If[RandomReal[] < p, grid[[nb[[1]], nb[[2]]]], grid[[r, c]]]]],
      {r, L}, {c, L}];
    AppendTo[frames, grid],
    {g, gens}];
  frames];

Print["[movie] building score tables ..."];
Tlo = scoreTable[0.02]; Thi = scoreTable[0.15];
Print["[movie] running low- and high-noise lattices (", gens, " gens, ", L, "x", L, ") ..."];
framesLo = runFrames[0.02, Tlo];
framesHi = runFrames[0.15, Thi];

coopFrac[grid_] := N@Mean@Flatten[grid /. {2 -> 0, _?Positive -> 1}]; (* AllD=2 -> defect *)
allDidx = 2;

gridImg[grid_, sz_] := ArrayPlot[grid,
  ColorRules -> Thread[Range[nS] -> cols], Frame -> False, Mesh -> False,
  ImageSize -> sz, PlotRangePadding -> 0];

legend = SwatchLegend[cols, paletteNames, LegendLayout -> "Row", LegendMarkerSize -> 12];

(* ---- single high-noise movie -------------------------------------- *)
Print["[movie] rendering high-noise GIF ..."];
hiFrames = Table[
  Labeled[gridImg[framesHi[[g]], 360],
    Row[{Style["Spatial IPD, \[Epsilon]=0.15   gen " <> ToString[g - 1], 12],
      "    coop=", Style[ToString[NumberForm[coopFrac[framesHi[[g]]], {3, 2}]], Bold]}],
    Top],
  {g, 1, Length[framesHi], 1}];
Export[FileNameJoin[{imgDir, "spatial_movie.gif"}],
  hiFrames, "GIF", "DisplayDurations" -> 0.12, AnimationRepetitions -> Infinity,
  ImageResolution -> 70];

(* ---- side-by-side low vs high noise -------------------------------- *)
Print["[movie] rendering side-by-side comparison GIF ..."];
cmpFrames = Table[
  Column[{
    Row[{
      Labeled[gridImg[framesLo[[g]], 250], Style["\[Epsilon] = 0.02 (low noise)", 11], Top],
      Spacer[24],
      Labeled[gridImg[framesHi[[g]], 250], Style["\[Epsilon] = 0.15 (high noise)", 11], Top]}],
    Style["generation " <> ToString[g - 1], 11, GrayLevel[0.4]],
    legend}, Alignment -> Center, Spacings -> 1],
  {g, 1, Length[framesHi], 2}];
Export[FileNameJoin[{imgDir, "spatial_movie_compare.gif"}],
  cmpFrames, "GIF", "DisplayDurations" -> 0.16, AnimationRepetitions -> Infinity,
  ImageResolution -> 70];

(* committed frames for reproducibility (subsample to keep size sane) *)
Put[<|"palette" -> paletteNames, "L" -> L, "gens" -> gens,
   "framesLo" -> framesLo[[;; ;; 4]], "framesHi" -> framesHi[[;; ;; 4]]|>,
  FileNameJoin[{dataDir, "spatial_movie_frames.m"}]];

Print["[movie] done. final coop: low=", NumberForm[coopFrac[Last@framesLo], {3, 2}],
  "  high=", NumberForm[coopFrac[Last@framesHi], {3, 2}]];
Print["[movie] wrote spatial_movie.gif + spatial_movie_compare.gif"];
