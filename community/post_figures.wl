(* ::Package:: *)

(* ===================================================================== *)
(* post_figures.wl -- every figure in the Axelrod Community post, as a     *)
(* function that reads committed data/ files and RETURNS the graphic.      *)
(*                                                                         *)
(* Loaded by the notebook's initialization cell:                           *)
(*    SetDirectory[NotebookDirectory[]];                                   *)
(*    Get["post_figures.wl"];                                              *)
(* Then each figure in the post is produced by a visible call, e.g.        *)
(*    PostFigures`replicatorVsNoise[]                                       *)
(*                                                                         *)
(* Data path resolution: looks for ../data relative to this file, or a     *)
(* sibling data/ folder, so it works whether run from community/ or with   *)
(* the data folder placed next to the notebook.                            *)
(* ===================================================================== *)

BeginPackage["PostFigures`", {"Axelrod`"}];

ClearAll["PostFigures`*", "PostFigures`Private`*"];

$dataDir::usage = "$dataDir is the folder figure functions read CSV/MX data from.";
setDataDir::usage = "setDataDir[dir] sets the data folder explicitly.";

replicatorVsNoise::usage   = "replicatorVsNoise[] -- evolutionarily stable mix vs noise (replicator).";
moranVsNoise::usage        = "moranVsNoise[] -- long-run composition vs noise (Moran).";
replicatorTrajLow::usage   = "replicatorTrajLow[] -- replicator trajectory at eps=0.";
replicatorTrajHigh::usage  = "replicatorTrajHigh[] -- replicator trajectory at eps=0.2.";
payoffHeatmap::usage       = "payoffHeatmap[] -- mean per-round payoff matrix (eps=0).";
scoreVsNoise::usage        = "scoreVsNoise[] -- every strategy's field score vs noise.";
rankingGrid::usage         = "rankingGrid[] -- ranking bar charts at four noise levels.";
discoveryProgress::usage   = "discoveryProgress[] -- greedy hill-climb best-so-far.";
discoveredVsTFT::usage     = "discoveredVsTFT[] -- discovered memory-one strategy vs TFT.";
discoveredParams::usage    = "discoveredParams[] -- discovered memory-one probabilities vs noise.";
fsmVsTFT::usage            = "fsmVsTFT[] -- evolved finite-state machines vs TFT.";
fsmComplexity::usage       = "fsmComplexity[] -- evolved machine complexity vs noise.";
spatialComposition::usage  = "spatialComposition[] -- lattice composition vs noise.";
spatialSnapshots::usage    = "spatialSnapshots[] -- lattice snapshots, low vs high noise.";
spatialLegend::usage       = "spatialLegend[] -- colour legend for the lattice figures.";
coevolutionTraj::usage     = "coevolutionTraj[] -- Red-Queen cooperativeness over generations.";
coevolutionGenome::usage   = "coevolutionGenome[] -- co-evolved mean genome vs noise.";
essStabilityMap::usage     = "essStabilityMap[] -- which strategies are ESS, vs noise.";
essInvasibilityHigh::usage = "essInvasibilityHigh[] -- invasibility matrix at eps=0.2.";
championVsTFT::usage       = "championVsTFT[] -- MemTwo-Long vs TFT vs best non-champion, vs noise.";
championRanking::usage     = "championRanking[] -- field score of all 26 strategies, champions highlighted.";
basinsGrid::usage          = "basinsGrid[] -- replicator basins of attraction on the 3-strategy simplex, vs noise.";
fingerprintsGrid::usage    = "fingerprintsGrid[] -- Ashlock fingerprints of four strategies at eps=0.";
spatialMovieFrame::usage   = "spatialMovieFrame[g] -- one frame of the spatial 'cooperation weather map'; spatialMovieFrame[] = last.";

Begin["`Private`"];

(* ---- data location ------------------------------------------------- *)
$here = If[StringQ[$InputFileName] && $InputFileName =!= "",
   DirectoryName[$InputFileName], Directory[]];
findData[] := Module[{c},
  c = {FileNameJoin[{ParentDirectory[$here], "data"}],
       FileNameJoin[{$here, "data"}],
       FileNameJoin[{Directory[], "data"}]};
  SelectFirst[c, DirectoryQ, First[c]]];
$dataDir = findData[];
setDataDir[d_] := ($dataDir = d);
dpath[f_] := FileNameJoin[{$dataDir, f}];
csv[f_]  := Import[dpath[f], "CSV"];

(* ---- shared palette: one colour per classic strategy --------------- *)
names = Axelrod`strategyNames;
nN    = Length[names];
palette   = Table[ColorData["Rainbow"][(i - 1)/(nN - 1)], {i, nN}];
nameColor = AssociationThread[names -> palette];
swatches[which_: names] := SwatchLegend[nameColor /@ which, which,
  LegendLayout -> "Column", LegendMarkerSize -> 14];

(* ---- composition / trajectory stacked-area helpers ----------------- *)
stackFromCSV[file_, xlabel_, label_] := Module[{raw, x, series},
  raw = csv[file]; x = N@raw[[2 ;;, 1]]; series = Transpose[N@raw[[2 ;;, 2 ;;]]];
  StackedListPlot[Table[Transpose[{x, series[[i]]}], {i, nN}],
    PlotStyle -> palette, PlotRange -> {{Min[x], Max[x]}, {0, 1}},
    Filling -> Automatic, Frame -> True,
    FrameLabel -> {xlabel, "population fraction"}, PlotLabel -> label,
    ImageSize -> 720, AspectRatio -> 0.5, PlotLegends -> swatches[]]];

replicatorVsNoise[] := stackFromCSV["replicator_vs_noise.csv",
  "execution noise \[Epsilon]", "Evolutionarily stable mix vs noise (replicator dynamics)"];
moranVsNoise[] := stackFromCSV["moran_vs_noise.csv",
  "execution noise \[Epsilon]", "Long-run composition vs noise (Moran process, N=100)"];
replicatorTrajLow[]  := stackFromCSV["replicator_traj_lownoise.csv",
  "generation", "Replicator trajectory, \[Epsilon]=0"];
replicatorTrajHigh[] := stackFromCSV["replicator_traj_highnoise.csv",
  "generation", "Replicator trajectory, \[Epsilon]=0.2"];

(* ---- tournament figures (read the committed matrix_noise*.csv) ------ *)
matAt[eps_] := Module[{raw},
  raw = csv["matrix_noise" <> eps <> ".csv"];
  N@raw[[2 ;;, 2 ;;]]];
noiseTags = {"0.", "0.01", "0.05", "0.1"};
noiseVals = {0., 0.01, 0.05, 0.1};

payoffHeatmap[] := MatrixPlot[matAt["0."], ColorFunction -> "TemperatureMap",
  FrameTicks -> {{Table[{i, names[[i]]}, {i, nN}], None},
     {None, Table[{j, Rotate[names[[j]], 90 Degree]}, {j, nN}]}},
  PlotLabel -> "Mean per-round payoff: row vs column (\[Epsilon]=0)",
  ImageSize -> 760, PlotLegends -> Automatic];

scoreVsNoise[] := Module[{series},
  series = Table[
    Transpose[{noiseVals, Table[Mean[matAt[noiseTags[[k]]][[i]]], {k, Length@noiseVals}]}],
    {i, nN}];
  ListLinePlot[series, PlotStyle -> palette, PlotMarkers -> Automatic,
    Frame -> True, FrameLabel -> {"execution noise \[Epsilon]", "mean field score"},
    PlotLabel -> "Strategy field score vs noise",
    PlotLegends -> swatches[], ImageSize -> 760]];

rankingGrid[] := Module[{bars},
  bars = Table[
    With[{rk = Axelrod`ranking[names, matAt[noiseTags[[k]]]]},
     BarChart[Reverse[rk[[All, 3]]],
       ChartLabels -> Placed[Reverse[rk[[All, 2]]], Axis, Rotate[#, 90 Degree] &],
       ChartStyle -> (nameColor /@ Reverse[rk[[All, 2]]]), Frame -> True,
       PlotLabel -> "Field score, \[Epsilon]=" <> noiseTags[[k]],
       PlotRange -> {1.6, 2.8}, ImageSize -> 460]],
    {k, Length@noiseTags}];
  GraphicsGrid[Partition[bars, 2], ImageSize -> 940]];

(* ---- discovery (memory-one) ---------------------------------------- *)
discData[] := Module[{d = csv["discovered_strategies.csv"]},
  <|"noise" -> N@d[[2 ;;, 1]], "params" -> N@d[[2 ;;, 2 ;; 6]],
    "fit" -> N@d[[2 ;;, 7]], "tft" -> N@d[[2 ;;, 8]]|>];

discoveredVsTFT[] := Module[{d = discData[]},
  ListLinePlot[{Transpose[{d["noise"], d["fit"]}], Transpose[{d["noise"], d["tft"]}]},
    PlotStyle -> {Directive[Thick, ColorData[97][1]],
                  Directive[Thick, Dashed, ColorData[97][4]]},
    PlotMarkers -> Automatic, Frame -> True,
    FrameLabel -> {"execution noise \[Epsilon]", "mean field score"},
    PlotLabel -> "Discovered strategy beats Tit-for-Tat at every noise level",
    PlotLegends -> {"discovered", "Tit-for-Tat"}, ImageSize -> 720]];

discoveredParams[] := Module[{d = discData[], pn, pcols},
  pn = {"open", "pCC", "pCD", "pDC", "pDD"};
  pcols = Table[ColorData["DarkRainbow"][(i - 1)/4], {i, 5}];
  ListLinePlot[Table[Transpose[{d["noise"], d["params"][[All, i]]}], {i, 5}],
    PlotStyle -> pcols, PlotMarkers -> Automatic, PlotRange -> {0, 1},
    Frame -> True, FrameLabel -> {"execution noise \[Epsilon]", "optimal memory-one probability"},
    PlotLabel -> "Discovered best-response strategy vs noise",
    PlotLegends -> pn, ImageSize -> 720]];

bestSoFar[file_] := Module[{raw = Import[dpath[file], "TSV"], fit},
  fit = N@raw[[2 ;;, 2]]; FoldList[Max, First[fit], Rest[fit]]];
discoveryProgress[] := ListLinePlot[
  {bestSoFar["discovery_log_eps0.tsv"], bestSoFar["discovery_log_eps0.1.tsv"]},
  PlotStyle -> {ColorData[97][1], ColorData[97][2]}, Frame -> True,
  FrameLabel -> {"proposal #", "best field score so far"},
  PlotLabel -> "Strategy discovery (greedy propose / evaluate / accept)",
  PlotLegends -> {"\[Epsilon]=0", "\[Epsilon]=0.1"}, ImageSize -> 720];

(* ---- FSM ----------------------------------------------------------- *)
fsmData[] := Module[{d = csv["fsm_discovered.csv"]},
  <|"noise" -> N@d[[2 ;;, 1]], "fit" -> N@d[[2 ;;, 2]], "tft" -> N@d[[2 ;;, 3]],
    "states" -> N@d[[2 ;;, 5]]|>];
fsmVsTFT[] := Module[{d = fsmData[]},
  ListLinePlot[{Transpose[{d["noise"], d["fit"]}], Transpose[{d["noise"], d["tft"]}]},
    PlotStyle -> {Directive[Thick, ColorData[97][1]],
                  Directive[Thick, Dashed, ColorData[97][4]]},
    PlotMarkers -> Automatic, Frame -> True,
    FrameLabel -> {"execution noise \[Epsilon]", "mean field score"},
    PlotLabel -> "Evolved finite-state machines vs Tit-for-Tat",
    PlotLegends -> {"evolved FSM", "Tit-for-Tat"}, ImageSize -> 700]];
fsmComplexity[] := Module[{d = fsmData[]},
  ListLinePlot[Transpose[{d["noise"], d["states"]}],
    PlotStyle -> Directive[Thick, ColorData[97][3]], PlotMarkers -> Automatic,
    Frame -> True, PlotRange -> {0, 4.5},
    FrameLabel -> {"execution noise \[Epsilon]", "reachable states of best machine"},
    PlotLabel -> "Evolved strategy complexity vs noise", ImageSize -> 700]];

(* ---- spatial ------------------------------------------------------- *)
spatialPalette = {"AllC", "AllD", "TitForTat", "GenerousTFT", "Pavlov", "Grim"};
spatialCols = Table[ColorData["Rainbow"][(i - 1)/(Length[spatialPalette] - 1)],
  {i, Length[spatialPalette]}];
spatialComposition[] := Module[{raw, x, series},
  raw = csv["spatial_vs_noise.csv"]; x = N@raw[[2 ;;, 1]];
  series = Transpose[N@raw[[2 ;;, 2 ;;]]];
  StackedListPlot[Table[Transpose[{x, series[[i]]}], {i, Length[spatialPalette]}],
    PlotStyle -> spatialCols, Filling -> Automatic,
    PlotRange -> {{0, Max[x]}, {0, 1}}, Frame -> True,
    FrameLabel -> {"execution noise \[Epsilon]", "lattice fraction"},
    PlotLabel -> "Spatial evolution: final composition vs noise",
    PlotLegends -> SwatchLegend[spatialCols, spatialPalette],
    ImageSize -> 760, AspectRatio -> 0.5]];
spatialLegend[] := SwatchLegend[spatialCols, spatialPalette, LegendLayout -> "Row"];
spatialSnapshots[] := Module[{s = Import[dpath["spatial_snapshots.m"]], lo, hi, g, pal, cells},
  pal = s["palette"]; g = s["gens"];
  lo = s["snaps"][0.]; hi = s["snaps"][Max[Keys[s["snaps"]]]];
  cells = Function[{grid, lbl},
    Labeled[ArrayPlot[grid, ColorRules -> Thread[Range[Length[pal]] -> spatialCols],
      Frame -> False, ImageSize -> 200, Mesh -> False, PlotRangePadding -> 0], lbl, Top]];
  Grid[{
    {Style["\[Epsilon] = 0", Bold], SpanFromLeft, SpanFromLeft},
    {cells[lo[[1]], "gen 1"], cells[lo[[2]], "gen " <> ToString[Round[g/2]]],
     cells[lo[[3]], "gen " <> ToString[g]]},
    {Style["\[Epsilon] = " <> ToString[Max[Keys[s["snaps"]]]], Bold], SpanFromLeft, SpanFromLeft},
    {cells[hi[[1]], "gen 1"], cells[hi[[2]], "gen " <> ToString[Round[g/2]]],
     cells[hi[[3]], "gen " <> ToString[g]]}},
   Frame -> All, Spacings -> {1.5, 1.5}]];

(* ---- co-evolution -------------------------------------------------- *)
coevolutionTraj[] := Module[{raw = csv["coevolution_coop_traj.csv"], lo, hi},
  lo = N@raw[[2 ;;, 2]]; hi = N@raw[[2 ;;, 3]];
  ListLinePlot[{lo, hi},
    PlotStyle -> {Directive[Thick, ColorData[97][1]], Directive[Thick, ColorData[97][4]]},
    PlotLegends -> {"\[Epsilon]=0", "\[Epsilon]=0.15"}, Frame -> True,
    PlotRange -> {0, 1}, FrameLabel -> {"generation", "population-mean pCC (cooperativeness)"},
    PlotLabel -> "Red-Queen co-evolution: cooperation climbs at low noise, restless at high",
    ImageSize -> 760]];
coevolutionGenome[] := Module[{raw = csv["coevolution_final_genome_vs_noise.csv"], x, pn, pcols},
  x = N@raw[[2 ;;, 1]]; pn = {"open", "pCC", "pCD", "pDC", "pDD"};
  pcols = Table[ColorData["DarkRainbow"][(i - 1)/4], {i, 5}];
  ListLinePlot[Table[Transpose[{x, N@raw[[2 ;;, i + 1]]}], {i, 5}],
    PlotStyle -> pcols, PlotMarkers -> Automatic, PlotRange -> {0, 1},
    PlotLegends -> pn, Frame -> True,
    FrameLabel -> {"execution noise \[Epsilon]", "co-evolved mean genome"},
    PlotLabel -> "Co-evolved population genome vs noise", ImageSize -> 760]];

(* ---- ESS ----------------------------------------------------------- *)
essStabilityMap[] := Module[{raw, sweep, pNames, stableSets, grid, cell},
  raw = csv["ess_stable_vs_noise.csv"];
  sweep = N@raw[[2 ;;, 1]];
  (* a noise level with no ESS imports as an empty string / Missing[] *)
  cell[x_] := If[StringQ[x] && StringTrim[x] =!= "",
     StringTrim /@ StringSplit[x, ";"], {}];
  stableSets = cell /@ raw[[2 ;;, 2]];
  pNames = Union @ Flatten[stableSets];
  grid = Table[Boole@MemberQ[stableSets[[j]], pNames[[i]]],
    {i, Length[pNames]}, {j, Length[sweep]}];
  MatrixPlot[grid, ColorRules -> {0 -> White, 1 -> RGBColor[0.18, 0.5, 0.2]},
    FrameTicks -> {{Table[{i, pNames[[i]]}, {i, Length[pNames]}], None},
       {None, Table[{j, sweep[[j]]}, {j, Length[sweep]}]}},
    Mesh -> All, FrameLabel -> {"", "execution noise \[Epsilon]"},
    PlotLabel -> "Evolutionarily stable strategies vs noise (green = no mutant invades)",
    ImageSize -> 620]];

essInvasibilityHigh[] := Module[{raw, lbls, mat},
  raw = csv["ess_invmatrix_eps0.2.csv"];
  lbls = raw[[1, 2 ;;]]; mat = N@raw[[2 ;;, 2 ;;]];
  MatrixPlot[mat, ColorRules -> {0 -> Lighter[Gray, 0.7], 1 -> RGBColor[0.78, 0.18, 0.18]},
    FrameTicks -> {{Table[{i, lbls[[i]]}, {i, Length[lbls]}], None},
       {None, Table[{j, Rotate[lbls[[j]], 90 Degree]}, {j, Length[lbls]}]}},
    PlotLabel -> "Invasibility (row invades column), \[Epsilon]=0.2", ImageSize -> 620]];

(* ---- champion (memory-two, 8h run) --------------------------------- *)
champData[] := Module[{raw = csv["champion_ranking_vs_noise.csv"]},
  <|"strat" -> (StringReplace[#, "*" -> ""] & /@ raw[[2 ;;, 2]]),
    "isCh" -> (N@raw[[2 ;;, 3]] /. {1. -> True, 0. -> False}),
    "sweep" -> N@raw[[2 ;;, 4]], "perN" -> N@raw[[2 ;;, 5 ;;]],
    "noises" -> N@ToExpression[StringReplace[#, "eps_" -> ""] & /@ raw[[1, 5 ;;]]]|>];

championRanking[] := Module[{d = champData[], star = "\[FivePointedStar]", lbl, cols},
  lbl = MapThread[If[#2, star <> " " <> #1, #1] &, {d["strat"], d["isCh"]}];
  cols = If[#, RGBColor[0.85, 0.33, 0.1], GrayLevel[0.62]] & /@ d["isCh"];
  BarChart[Reverse[d["sweep"]],
    ChartLabels -> Placed[Reverse[lbl], Axis, Rotate[#, 90 Degree] &],
    ChartStyle -> Reverse[cols], Frame -> True, PlotRange -> {2.0, 2.65},
    PlotLabel -> Style["Field score, mean over noise (" <> star <> " = discovered champion)", 14],
    ImageSize -> 940, AspectRatio -> 0.52]];

championVsTFT[] := Module[{d = champData[], posOf, bestNonCh, trio, tcol},
  posOf[nm_] := First@FirstPosition[d["strat"], nm];
  bestNonCh = d["strat"][[ First@Select[Range@Length@d["strat"], ! d["isCh"][[#]] &] ]];
  trio = {"MemTwo-Long", "TitForTat", bestNonCh};
  tcol = {RGBColor[0.85, 0.33, 0.1], RGBColor[0.2, 0.4, 0.75], GrayLevel[0.45]};
  ListLinePlot[Table[Transpose[{d["noises"], d["perN"][[posOf[nm]]]}], {nm, trio}],
    PlotStyle -> (Directive[Thick, #] & /@ tcol), PlotMarkers -> Automatic,
    PlotLegends -> {"MemTwo-Long (8h champion)", "Tit-for-Tat", bestNonCh <> " (best non-champion)"},
    Frame -> True, FrameLabel -> {"execution noise \[Epsilon]", "mean field score"},
    PlotLabel -> "Discovered memory-two champion vs Tit-for-Tat across noise",
    ImageSize -> 760]];

(* ---- special features (read committed .m data, return graphics) ----- *)

(* basins of attraction on the {AllD, TitForTat, GenerousTFT} simplex *)
basinsGrid[] := Module[{d, v, names, triCols, pts, plot},
  d = Import[dpath["basins.m"]];
  {v, names} = {d["v"], d["names"]};
  triCols = {RGBColor[0.75, 0.2, 0.2], RGBColor[0.2, 0.4, 0.75], RGBColor[0.2, 0.6, 0.3]};
  plot[eps_] := Graphics[{PointSize[0.012],
     {triCols[[#[[2]]]], Point[#[[1]]]} & /@ d["points"][eps],
     Black, Thick, Line[{v[[1]], v[[2]], v[[3]], v[[1]]}],
     Text[Style[names[[1]], 11, triCols[[1]]], v[[1]] - {0.04, 0.03}],
     Text[Style[names[[2]], 11, triCols[[2]]], v[[2]] + {0.04, -0.03}],
     Text[Style[names[[3]], 11, triCols[[3]]], v[[3]] + {0, 0.04}]},
    PlotLabel -> Style["basins, \[Epsilon]=" <> ToString[eps], 13],
    ImageSize -> 360, PlotRange -> {{-0.12, 1.12}, {-0.1, 1.0}}];
  Grid[Partition[plot /@ d["noise"], 3], Spacings -> {0.5, 0.5}]];

(* Ashlock fingerprints at eps=0 *)
fingerprintsGrid[] := Module[{d, allv, lo, hi, fp},
  d = Import[dpath["fingerprint_eps0.m"]];
  allv = Select[Flatten[d["surfaces"]], NumericQ]; lo = Min[allv]; hi = Max[allv];
  fp[surf_, label_] := ArrayPlot[surf, DataReversed -> True,
    ColorFunction -> (ColorData["TemperatureMap"][Rescale[#, {lo, hi}]] &),
    ColorFunctionScaling -> False, ColorRules -> {_Missing -> GrayLevel[0.85]},
    Frame -> True, FrameTicks -> None, PlotLabel -> Style[label, 14],
    ImageSize -> 320, PlotLegends -> Automatic];
  Grid[Partition[MapThread[fp, {d["surfaces"], d["names"]}], 2], Spacings -> {1, 1}]];

(* one frame of the spatial cooperation movie (committed subsampled grids) *)
spatialMovieFrame[gen_: Automatic] := Module[{d, pal, cols, fr, g, img},
  d = Import[dpath["spatial_movie_frames.m"]];
  pal = d["palette"];
  cols = Append[Table[ColorData["Rainbow"][(i - 1)/(Length[pal] - 2)], {i, Length[pal] - 1}],
     RGBColor[0.85, 0.33, 0.1]];
  fr = d["framesHi"]; g = If[gen === Automatic, Length[fr], Clip[gen, {1, Length[fr]}]];
  img[grid_] := ArrayPlot[grid, ColorRules -> Thread[Range[Length[pal]] -> cols],
    Frame -> False, Mesh -> False, ImageSize -> 360, PlotRangePadding -> 0];
  Column[{img[fr[[g]]],
     SwatchLegend[cols, pal, LegendLayout -> "Row", LegendMarkerSize -> 11]},
    Alignment -> Center]];

End[];
EndPackage[];
