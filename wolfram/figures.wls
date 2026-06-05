#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  figures.wls -- regenerate ALL publication figures from the data files
    in data/ (run tournament.wls and evolution.wls and discover.wls first).
    Single source of truth for figures; cheap to re-run.  Outputs PNGs to
    docs/images/.                                                          *)

Get[FileNameJoin[{DirectoryName[$InputFileName], "axelrod.wl"}]];
repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
dataDir  = FileNameJoin[{repoRoot, "data"}];
imgDir   = FileNameJoin[{repoRoot, "docs", "images"}];
If[! DirectoryQ[imgDir], CreateDirectory[imgDir, CreateIntermediateDirectories -> True]];

names = Axelrod`strategyNames;
n     = Length[names];

(* Readable axis & label defaults: same fonts as post_figures.wl so the
   PNG outputs and the in-notebook evaluated graphics match visually. *)
$baseStyle  = {FontFamily -> "Helvetica", FontSize -> 13};
$labelStyle = Directive[FontSize -> 15, FontFamily -> "Helvetica",
                        FontColor -> GrayLevel[0.18]];
SetOptions[#, BaseStyle -> $baseStyle, LabelStyle -> $labelStyle] & /@ {
  ListLinePlot, ListPlot, StackedListPlot, BarChart, MatrixPlot,
  ArrayPlot, Graphics, GraphicsGrid};

(* Behaviour-grouped categorical palette: cooperator (blue), forgiving
   reciprocators (teal/green), strict reciprocators (gold/orange),
   punitive reciprocators (burnt), exploiters (red/magenta), Random (grey).
   Replaces the perceptually-flat "Rainbow" ramp. *)
nameColor = <|
  "AllC"          -> RGBColor[0.22, 0.45, 0.72],
  "GenerousTFT"   -> RGBColor[0.27, 0.66, 0.60],
  "ZD-Generous"   -> RGBColor[0.45, 0.80, 0.73],
  "TitFor2Tats"   -> RGBColor[0.40, 0.74, 0.36],
  "2TitsForTat"   -> RGBColor[0.60, 0.78, 0.32],
  "Pavlov"        -> RGBColor[0.78, 0.78, 0.30],
  "Gradual"       -> RGBColor[0.86, 0.70, 0.27],
  "TitForTat"     -> RGBColor[0.92, 0.60, 0.22],
  "SuspiciousTFT" -> RGBColor[0.83, 0.43, 0.27],
  "Grim"          -> RGBColor[0.65, 0.30, 0.25],
  "AllD"          -> RGBColor[0.80, 0.18, 0.20],
  "ZD-Extort-2"   -> RGBColor[0.48, 0.16, 0.34],
  "Random"        -> GrayLevel[0.55]
|>;
palette = nameColor /@ names;

(* Stack order (bottom -> top) tells a story:
   cooperators -> forgiving reciprocators -> strict reciprocators ->
   exploiters -> noise. Keeps similar behaviours adjacent in the band. *)
stackOrder = {"AllC", "GenerousTFT", "ZD-Generous", "TitFor2Tats",
              "2TitsForTat", "Pavlov", "Gradual", "TitForTat",
              "SuspiciousTFT", "Grim", "AllD", "ZD-Extort-2", "Random"};

(* Strategies whose max share never reaches this fraction are drawn as
   a faint grey band and collapsed into a single 'trace' legend entry. *)

swatches[which_: names] := SwatchLegend[nameColor /@ which, which,
  LegendLayout -> "Column", LegendMarkerSize -> 14];

(* Solid stacked-area: draw cumulative curves from the top band down, each
   filled to the axis with an OPAQUE colour so lower bands over-paint the
   ones above. Avoids StackedListPlot's translucent Filling (which left
   muddy fills + white seams). Band i shows the colour of ord[[i]]. *)
solidStack[x_, ord_, series_, xLabel_, yLabel_, label_, imgSize_, aspect_] :=
  Module[{k = Length @ ord, cum, draw},
  cum  = Accumulate[series];
  draw = Range[k, 1, -1];
  ListLinePlot[
    Table[Transpose[{x, cum[[j]]}], {j, draw}],
    PlotStyle  -> Table[Directive[nameColor[ord[[j]]], AbsoluteThickness[0.8]], {j, draw}],
    Filling      -> Axis,
    FillingStyle -> Table[p -> Directive[Opacity[1], nameColor[ord[[draw[[p]]]]]], {p, k}],
    PlotRange  -> {{Min[x], Max[x]}, {0, 1}}, Frame -> True,
    FrameLabel -> {xLabel, yLabel}, PlotLabel -> label,
    ImageSize  -> imgSize, AspectRatio -> aspect,
    PlotLegends -> SwatchLegend[nameColor /@ Reverse[ord], Reverse[ord],
       LegendLayout -> "Column", LegendMarkerSize -> 14]]];

stackArea[file_, label_, xLabel_: "execution noise \[Epsilon]"] := Module[
   {raw, hdr, x, byName, ord, series},
  raw    = Import[FileNameJoin[{dataDir, file}], "CSV"];
  hdr    = raw[[1, 2 ;;]];
  x      = N @ raw[[2 ;;, 1]];
  byName = AssociationThread[hdr -> Transpose[N @ raw[[2 ;;, 2 ;;]]]];
  ord    = Select[stackOrder, KeyExistsQ[byName, #] &];
  series = byName /@ ord;
  solidStack[x, ord, series, xLabel, "population fraction", label, 720, 0.5]
];

Print["[figures] evolution: replicator + Moran composition..."];
Export[FileNameJoin[{imgDir, "replicator_vs_noise.png"}],
  stackArea["replicator_vs_noise.csv",
    "Evolutionarily stable mix vs noise (replicator dynamics)"],
  ImageResolution -> 144];
Export[FileNameJoin[{imgDir, "moran_vs_noise.png"}],
  stackArea["moran_vs_noise.csv",
    "Long-run composition vs noise (Moran process, N=100)"],
  ImageResolution -> 144];

(* representative replicator trajectories (generation -> composition) *)
Export[FileNameJoin[{imgDir, "replicator_traj_lownoise.png"}],
  stackArea["replicator_traj_lownoise.csv",
    "Replicator trajectory, \[Epsilon]=0", "generation"],
  ImageResolution -> 144];
Export[FileNameJoin[{imgDir, "replicator_traj_highnoise.png"}],
  stackArea["replicator_traj_highnoise.csv",
    "Replicator trajectory, \[Epsilon]=0.2", "generation"],
  ImageResolution -> 144];

(* ---- tournament: heatmap, score-vs-noise, ranking grid -------------
   Read matrices from the committed CSVs (the .mx file is regenerable
   and git-ignored, so don't depend on it).                            *)
noiseTags = {"0.", "0.01", "0.05", "0.1"};
noises    = {0., 0.01, 0.05, 0.1};
matFromCSV[tag_] := With[
    {raw = Import[FileNameJoin[{dataDir, "matrix_noise" <> tag <> ".csv"}], "CSV"]},
  N @ raw[[2 ;;, 2 ;;]]];
matrices = AssociationThread[noises -> matFromCSV /@ noiseTags];
d  = <|"noises" -> noises, "matrices" -> matrices|>;
M0 = matrices[0.];

Print["[figures] tournament: heatmap, score-vs-noise, ranking grid..."];
Export[FileNameJoin[{imgDir, "payoff_heatmap.png"}],
  MatrixPlot[M0, ColorFunction -> "TemperatureMap",
    FrameTicks -> {
      {Table[{i, names[[i]]}, {i, n}], None},
      {None, Table[{j, Rotate[names[[j]], 90 Degree]}, {j, n}]}},
    PlotLabel -> "Mean per-round payoff: row vs column (\[Epsilon]=0)",
    ImageSize -> 760, PlotLegends -> Automatic],
  ImageResolution -> 144];

scoreSeries = Table[
  Transpose[{noises, Table[Axelrod`fieldScores[d["matrices"][eps]][[i]], {eps, noises}]}],
  {i, n}];
Export[FileNameJoin[{imgDir, "score_vs_noise.png"}],
  ListLinePlot[scoreSeries, PlotStyle -> palette, PlotMarkers -> Automatic,
    Frame -> True, FrameLabel -> {"execution noise \[Epsilon]", "mean field score"},
    PlotLabel -> "Strategy field score vs noise",
    PlotLegends -> swatches[], ImageSize -> 760],
  ImageResolution -> 144];

rankBars[eps_] := Module[{rk = Axelrod`ranking[names, d["matrices"][eps]]},
  BarChart[Reverse[rk[[All, 3]]],
    ChartLabels -> Placed[Reverse[rk[[All, 2]]], Axis, Rotate[#, 90 Degree] &],
    ChartStyle -> (nameColor /@ Reverse[rk[[All, 2]]]), Frame -> True,
    PlotLabel -> "Field score, \[Epsilon]=" <> ToString[eps],
    PlotRange -> {1.6, 2.8}, ImageSize -> 460]];
Export[FileNameJoin[{imgDir, "ranking_grid.png"}],
  GraphicsGrid[Partition[rankBars /@ noises, 2], ImageSize -> 940],
  ImageResolution -> 144];

(* ---- discovery: progress + discovered params vs noise -------------- *)
Print["[figures] discovery: progress + params-vs-noise..."];
disc = Import[FileNameJoin[{dataDir, "discovered_strategies.csv"}], "CSV"];
dnoise = disc[[2 ;;, 1]];
dparams = disc[[2 ;;, 2 ;; 6]];
dfit   = disc[[2 ;;, 7]];
dtft   = disc[[2 ;;, 8]];
paramNames = {"open", "pCC", "pCD", "pDC", "pDD"};
pcols = Table[ColorData["DarkRainbow"][(i - 1)/4], {i, 5}];

Export[FileNameJoin[{imgDir, "discovered_params_vs_noise.png"}],
  ListLinePlot[
    Table[Transpose[{dnoise, dparams[[All, i]]}], {i, 5}],
    PlotStyle -> pcols, PlotMarkers -> Automatic, PlotRange -> {0, 1},
    Frame -> True, FrameLabel -> {"execution noise \[Epsilon]", "optimal memory-one probability"},
    PlotLabel -> "Discovered best-response strategy vs noise",
    PlotLegends -> paramNames, ImageSize -> 720],
  ImageResolution -> 144];

Export[FileNameJoin[{imgDir, "discovered_vs_tft.png"}],
  ListLinePlot[{Transpose[{dnoise, dfit}], Transpose[{dnoise, dtft}]},
    PlotStyle -> {Directive[Thick, ColorData[97][1]],
                  Directive[Thick, Dashed, ColorData[97][4]]},
    PlotMarkers -> Automatic, Frame -> True,
    FrameLabel -> {"execution noise \[Epsilon]", "mean field score"},
    PlotLabel -> "Discovered strategy beats Tit-for-Tat at every noise level",
    PlotLegends -> {"discovered", "Tit-for-Tat"}, ImageSize -> 720],
  ImageResolution -> 144];

(* greedy hill-climb progress (best-so-far) from the TSV logs *)
bestSoFar[file_] := Module[{raw, fit},
  raw = Import[FileNameJoin[{dataDir, file}], "TSV"];
  fit = N[raw[[2 ;;, 2]]];     (* fitness column is numeric in the TSV *)
  FoldList[Max, First[fit], Rest[fit]]];
Export[FileNameJoin[{imgDir, "discovery_progress.png"}],
  ListLinePlot[{bestSoFar["discovery_log_eps0.tsv"],
                bestSoFar["discovery_log_eps0.1.tsv"]},
    PlotStyle -> {ColorData[97][1], ColorData[97][2]},
    Frame -> True, FrameLabel -> {"proposal #", "best field score so far"},
    PlotLabel -> "Strategy discovery (greedy propose / evaluate / accept)",
    PlotLegends -> {"\[Epsilon]=0", "\[Epsilon]=0.1"}, ImageSize -> 720],
  ImageResolution -> 144];

Print["[figures] done -> ", imgDir];
