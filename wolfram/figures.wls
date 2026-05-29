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

(* stable, distinguishable colours, one per strategy *)
palette = Table[ColorData["Rainbow"][(i - 1)/(n - 1)], {i, n}];
nameColor = AssociationThread[names -> palette];
swatches[which_: names] := SwatchLegend[nameColor /@ which, which,
  LegendLayout -> "Column", LegendMarkerSize -> 14];

readComp[file_] := Module[{raw},
  raw = Import[FileNameJoin[{dataDir, file}], "CSV"];
  {raw[[2 ;;, 1]], raw[[2 ;;, 2 ;;]]}    (* {noiseGrid, fractionRows} *)
];

(* cumulative stacked-area chart of composition vs noise *)
stackArea[file_, label_] := Module[{eps, rows, series},
  {eps, rows} = readComp[file];
  series = Transpose[rows];                 (* one series per strategy *)
  StackedListPlot[
    Table[Transpose[{eps, series[[i]]}], {i, n}],
    PlotStyle -> palette, PlotRange -> {{Min[eps], Max[eps]}, {0, 1}},
    Filling -> Automatic, Frame -> True,
    FrameLabel -> {"execution noise \[Epsilon]", "population fraction"},
    PlotLabel -> label, ImageSize -> 720, AspectRatio -> 0.5,
    PlotLegends -> swatches[]]
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
stackTraj[file_, label_] := Module[{raw, gen, series},
  raw = Import[FileNameJoin[{dataDir, file}], "CSV"];
  gen = raw[[2 ;;, 1]]; series = Transpose[raw[[2 ;;, 2 ;;]]];
  StackedListPlot[Table[Transpose[{gen, series[[i]]}], {i, n}],
    PlotStyle -> palette, PlotRange -> {{Min[gen], Max[gen]}, {0, 1}},
    Filling -> Automatic, Frame -> True,
    FrameLabel -> {"generation", "population fraction"},
    PlotLabel -> label, ImageSize -> 720, AspectRatio -> 0.5,
    PlotLegends -> swatches[]]];
Export[FileNameJoin[{imgDir, "replicator_traj_lownoise.png"}],
  stackTraj["replicator_traj_lownoise.csv", "Replicator trajectory, \[Epsilon]=0"],
  ImageResolution -> 144];
Export[FileNameJoin[{imgDir, "replicator_traj_highnoise.png"}],
  stackTraj["replicator_traj_highnoise.csv", "Replicator trajectory, \[Epsilon]=0.2"],
  ImageResolution -> 144];

(* ---- tournament: heatmap, score-vs-noise, ranking grid ------------- *)
d      = Import[FileNameJoin[{dataDir, "tournament.mx"}]];
noises = d["noises"];
M0     = d["matrices"][0.];

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
