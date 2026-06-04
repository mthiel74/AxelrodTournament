#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  champion_analysis.wls -- evaluate the discovered champions (memory-one
    autoresearch champions + the 8h memory-two MemTwo-Long) alongside the
    classic + modern field, and produce:
      - data/champion_ranking_vs_noise.csv : field score of every strategy
        (classics, modern, champions) at each noise level, ranked
      - data/champion_field_scores.csv     : tidy long-form for plotting
      - docs/images/champion_ranking.png   : where the champions land
      - docs/images/champion_vs_tft_noise.png : MemTwo-Long vs TFT vs best
        classic, score across noise
    Honest protocol: rounds=200, reps=40, fresh seeds.                       *)

Get[FileNameJoin[{DirectoryName[$InputFileName], "axelrod.wl"}]];
Get[FileNameJoin[{DirectoryName[$InputFileName], "strategies_ext.wl"}]];
repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
dataDir  = FileNameJoin[{repoRoot, "data"}];
imgDir   = FileNameJoin[{repoRoot, "docs", "images"}];
Scan[If[! DirectoryQ[#], CreateDirectory[#, CreateIntermediateDirectories -> True]] &,
  {dataDir, imgDir}];

SeedRandom[31337];

(* ---- assemble the panel: classics + modern + champions ------------- *)
panel = <||>;
Do[panel[nm] = Axelrod`strategies[nm], {nm, Axelrod`strategyNames}];
Do[panel[nm] = StrategiesExt`extendedStrategies[nm],
  {nm, Keys[StrategiesExt`extendedStrategies]}];

champDir = FileNameJoin[{repoRoot, "autoresearch", "champions"}];
champNames = {};
If[DirectoryQ[champDir],
  Do[Module[{nm = "*" <> FileBaseName[f], sym = Unique["champ$"]},
      Block[{candidate}, Get[f];
        DownValues[Evaluate[sym]] = DownValues[candidate] /. candidate -> sym];
      panel[nm] = sym; AppendTo[champNames, nm]],
    {f, Sort@FileNames["*.wl", champDir]}]];

names = Keys[panel];
S = Values[panel];
nP = Length[names];
isChamp = MemberQ[champNames, #] &;
Print["[champ] panel = ", nP, " strategies (", Length[champNames],
  " discovered champions: ", champNames, ")"];

(* ---- field score of each strategy at each noise level -------------- *)
rounds = 200; reps = 40;
noises = {0., 0.02, 0.05, 0.1, 0.15, 0.2};

(* full mean-payoff matrix per noise, then row means = field score *)
fieldScoreAt[eps_] := Module[{M},
  M = Table[
    If[i == j, Axelrod`meanMatch[S[[i]], S[[i]], rounds, eps, reps][[1]],
      Axelrod`meanMatch[S[[i]], S[[j]], rounds, eps, reps][[1]]],
    {i, nP}, {j, nP}];
  Mean /@ M];

Print["[champ] scoring ", nP, " strategies over ", Length[noises], " noise levels ..."];
scoreByNoise = Association @ Table[
  Module[{t0 = AbsoluteTime[], sc},
    sc = fieldScoreAt[eps];
    Print["   eps=", NumberForm[eps, {4, 3}], "  (",
      Round[AbsoluteTime[] - t0, 0.1], "s)"];
    eps -> sc],
  {eps, noises}];

(* sweep score = mean over noise *)
sweepScore = Mean @ Table[scoreByNoise[eps], {eps, noises}];
order = Reverse @ Ordering[N@sweepScore];

(* ---- export ranking ----------------------------------------------- *)
Export[FileNameJoin[{dataDir, "champion_ranking_vs_noise.csv"}],
  Join[
    {Join[{"rank", "strategy", "isChampion", "sweep_mean"},
       ("eps_" <> ToString[#] &) /@ noises]},
    Table[
      Join[{r, names[[order[[r]]]], Boole@isChamp[names[[order[[r]]]]],
         N@Round[sweepScore[[order[[r]]]], 0.0001]},
        Table[N@Round[scoreByNoise[eps][[order[[r]]]], 0.0001], {eps, noises}]],
      {r, nP}]]];

Print["[champ] === overall ranking (sweep mean) ==="];
Do[Print["   ", StringPadLeft[ToString[r], 2], ". ",
   If[isChamp[names[[order[[r]]]]], "* ", "  "],
   StringPadRight[StringReplace[names[[order[[r]]]], "*" -> ""], 18],
   NumberForm[sweepScore[[order[[r]]]], {5, 4}]], {r, nP}];

(* ---- figure: ranking bar chart, champions highlighted ------------- *)
rankColors = Table[
  If[isChamp[names[[order[[r]]]]], RGBColor[0.85, 0.33, 0.1], GrayLevel[0.6]],
  {r, nP}];
Export[FileNameJoin[{imgDir, "champion_ranking.png"}],
  BarChart[Reverse@Table[sweepScore[[order[[r]]]], {r, nP}],
    ChartLabels -> Placed[
      Reverse@Table[StringReplace[names[[order[[r]]]], "*" -> "\[FilledStar]"], {r, nP}],
      Axis, Rotate[#, 90 Degree] &],
    ChartStyle -> Reverse[rankColors],
    Frame -> True, PlotRange -> {2.0, 2.6},
    PlotLabel -> "Field score (mean over noise) \[Dash] \[FilledStar] = discovered champion",
    ImageSize -> 900, AspectRatio -> 0.5],
  ImageResolution -> 144];

(* ---- figure: MemTwo-Long vs TFT vs best classic across noise ------- *)
idxOf[nm_] := First@FirstPosition[names, nm];
bestClassicName = First@Select[names[[order]], ! isChamp[#] &];
trio = {"*MemTwo-Long", "TitForTat", bestClassicName};
trioColors = {RGBColor[0.85, 0.33, 0.1], RGBColor[0.2, 0.4, 0.75], GrayLevel[0.45]};
Export[FileNameJoin[{imgDir, "champion_vs_tft_noise.png"}],
  ListLinePlot[
    Table[Transpose[{noises, Table[scoreByNoise[eps][[idxOf[nm]]], {eps, noises}]}],
      {nm, trio}],
    PlotStyle -> (Directive[Thick, #] & /@ trioColors),
    PlotMarkers -> Automatic,
    PlotLegends -> {"MemTwo-Long (8h champion)", "Tit-for-Tat",
       StringReplace[bestClassicName, "*" -> ""] <> " (best non-champion)"},
    Frame -> True, FrameLabel -> {"execution noise \[Epsilon]", "mean field score"},
    PlotLabel -> "Discovered memory-two champion vs Tit-for-Tat across noise",
    ImageSize -> 760],
  ImageResolution -> 144];

(* tidy long form *)
Export[FileNameJoin[{dataDir, "champion_field_scores.csv"}],
  Join[{{"strategy", "isChampion", "noise", "field_score"}},
    Flatten[Table[
      {StringReplace[names[[i]], "*" -> ""], Boole@isChamp[names[[i]]],
       eps, N@Round[scoreByNoise[eps][[i]], 0.0001]},
      {i, nP}, {eps, noises}], 1]]];

Print["[champ] done. MemTwo-Long rank = ",
  First@FirstPosition[names[[order]], "*MemTwo-Long"], " of ", nP];
Exit[0];
