#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  champion_figures.wls -- (re)build the champion figures FROM the saved
    data/champion_ranking_vs_noise.csv (no recomputation). Keeps figure styling
    separate from the expensive scoring in champion_analysis.wls.            *)

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
dataDir  = FileNameJoin[{repoRoot, "data"}];
imgDir   = FileNameJoin[{repoRoot, "docs", "images"}];

raw   = Import[FileNameJoin[{dataDir, "champion_ranking_vs_noise.csv"}], "CSV"];
hdr   = raw[[1]];
rows  = raw[[2 ;;]];
noises = ToExpression[StringReplace[#, "eps_" -> ""] & /@ hdr[[5 ;;]]];
(* rows already sorted by rank ascending (rank 1 = best) *)
strat  = StringReplace[#, "*" -> ""] & /@ rows[[All, 2]];
isCh   = rows[[All, 3]] /. {1 -> True, "1" -> True, 0 -> False, "0" -> False};
sweep  = N[rows[[All, 4]]];
perN   = N[rows[[All, 5 ;;]]];
star   = "\[FivePointedStar]";

(* ---- ranking bar chart, champions highlighted --------------------- *)
labels = MapThread[If[#2, star <> " " <> #1, #1] &, {strat, isCh}];
cols   = If[#, RGBColor[0.85, 0.33, 0.1], GrayLevel[0.62]] & /@ isCh;
Export[FileNameJoin[{imgDir, "champion_ranking.png"}],
  BarChart[Reverse[sweep],
    ChartLabels -> Placed[Reverse[labels], Axis, Rotate[#, 90 Degree] &],
    ChartStyle -> Reverse[cols], Frame -> True, PlotRange -> {2.0, 2.65},
    PlotLabel -> Style[
      "Field score, mean over noise (" <> star <> " = discovered champion)", 14],
    ImageSize -> 940, AspectRatio -> 0.52],
  ImageResolution -> 144];

(* ---- champion vs TFT vs best non-champion across noise ------------- *)
posOf[nm_] := First@FirstPosition[strat, nm];
bestNonCh  = strat[[ First@Select[Range[Length[strat]], ! isCh[[#]] &] ]];
trio = {"MemTwo-Long", "TitForTat", bestNonCh};
tcol = {RGBColor[0.85, 0.33, 0.1], RGBColor[0.2, 0.4, 0.75], GrayLevel[0.45]};
Export[FileNameJoin[{imgDir, "champion_vs_tft_noise.png"}],
  ListLinePlot[
    Table[Transpose[{noises, perN[[posOf[nm]]]}], {nm, trio}],
    PlotStyle -> (Directive[Thick, #] & /@ tcol), PlotMarkers -> Automatic,
    PlotLegends -> {"MemTwo-Long (8h champion)", "Tit-for-Tat",
       bestNonCh <> " (best non-champion)"},
    Frame -> True, FrameLabel -> {"execution noise \[Epsilon]", "mean field score"},
    PlotLabel -> "Discovered memory-two champion vs Tit-for-Tat across noise",
    ImageSize -> 760],
  ImageResolution -> 144];

Print["[champ-fig] rebuilt champion_ranking.png + champion_vs_tft_noise.png"];
Print["[champ-fig] MemTwo-Long rank = ", posOf["MemTwo-Long"], " of ", Length[strat],
  "  sweep=", sweep[[posOf["MemTwo-Long"]]]];
Exit[0];
