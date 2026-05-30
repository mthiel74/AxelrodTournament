#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  coevolution.wls -- Extension 4: a Red-Queen co-evolutionary arms race.

    Instead of optimising a strategy against a FIXED field, we co-evolve a
    population of memory-one strategies against ITSELF: each generation,
    every individual's fitness is its mean score against a sample of the
    current population (not a static benchmark). Selection + mutation then
    reshape the population, which changes next generation's fitness landscape
    -- the "Red Queen" effect.

    We track the population-mean cooperativeness over generations (low vs
    high noise) and the co-evolved mean genome across a noise sweep.

    Outputs: data/coevolution_*.csv, docs/images/coevolution_*.png           *)

Get[FileNameJoin[{DirectoryName[$InputFileName], "axelrod.wl"}]];
repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
dataDir  = FileNameJoin[{repoRoot, "data"}];
imgDir   = FileNameJoin[{repoRoot, "docs", "images"}];
Scan[If[! DirectoryQ[#], CreateDirectory[#, CreateIntermediateDirectories -> True]] &,
  {dataDir, imgDir}];

SeedRandom[1618];

popN     = 60;
gens     = 80;
rounds   = 80;
reps     = 2;
mutSigma = 0.06;
tourK    = 3;
genomeLen = 5;
sample   = 12;

clip[v_] := Clip[v, {0, 1}];
mk[g_] := Axelrod`memOne[g[[1]], g[[2 ;;]]];

popFitness[pop_, eps_] := Module[{nn = Length[pop], idx},
  Table[
    idx = RandomSample[Range[nn], UpTo[sample]];
    Mean@Table[Axelrod`meanMatch[mk[pop[[i]]], mk[pop[[j]]], rounds, eps, reps][[1]],
       {j, idx}],
    {i, nn}]
];

tournamentSelect[pop_, fits_] := Module[{cand},
  cand = RandomSample[Range[Length[pop]], tourK];
  pop[[ cand[[ First@Ordering[-fits[[cand]]] ]] ]]
];

coevolve[eps_] := Module[{pop, fits, coopTraj = {}, newpop},
  pop = RandomReal[{0, 1}, {popN, genomeLen}];
  Do[
    fits = popFitness[pop, eps];
    AppendTo[coopTraj, Mean[pop[[All, 2]]]];   (* mean pCC *)
    newpop = Table[
      clip[ (tournamentSelect[pop, fits] + tournamentSelect[pop, fits])/2
            + RandomVariate[NormalDistribution[0, mutSigma], genomeLen] ],
      {popN}];
    pop = newpop,
    {gens}];
  <|"finalGenome" -> Mean[pop], "coopTraj" -> coopTraj|>
];

Print["[coevo] co-evolving ", popN, " genomes for ", gens, " gens ..."];
resLow  = coevolve[0.];
resHigh = coevolve[0.15];

sweep = {0., 0.05, 0.1, 0.15, 0.2};
Print["[coevo] noise sweep ..."];
finalGenomes = Association@Table[
  Module[{t0 = AbsoluteTime[], r}, r = coevolve[eps];
    Print["   eps=", NumberForm[eps, {4, 3}],
      "  meanGenome=", Round[r["finalGenome"], 0.01],
      "  (", Round[AbsoluteTime[] - t0, 0.1], "s)"];
    eps -> r["finalGenome"]],
  {eps, sweep}];

paramNames = {"open", "pCC", "pCD", "pDC", "pDD"};
Export[FileNameJoin[{dataDir, "coevolution_final_genome_vs_noise.csv"}],
  Join[{Prepend[paramNames, "noise"]},
    Table[Prepend[N@Round[finalGenomes[eps], 0.0001], eps], {eps, sweep}]]];
Export[FileNameJoin[{dataDir, "coevolution_coop_traj.csv"}],
  Join[{{"generation", "meanPCC_lowNoise", "meanPCC_highNoise"}},
    Table[{g, N@Round[resLow["coopTraj"][[g]], 0.0001],
              N@Round[resHigh["coopTraj"][[g]], 0.0001]}, {g, gens}]]];

Export[FileNameJoin[{imgDir, "coevolution_coop_traj.png"}],
  ListLinePlot[{resLow["coopTraj"], resHigh["coopTraj"]},
    PlotStyle -> {Directive[Thick, ColorData[97][1]], Directive[Thick, ColorData[97][4]]},
    PlotLegends -> {"\[Epsilon]=0", "\[Epsilon]=0.15"},
    Frame -> True, PlotRange -> {0, 1},
    FrameLabel -> {"generation", "population-mean pCC (cooperativeness)"},
    PlotLabel -> "Red-Queen co-evolution: cooperation climbs at low noise, restless at high",
    ImageSize -> 760], ImageResolution -> 144];

pcols = Table[ColorData["DarkRainbow"][(i - 1)/4], {i, 5}];
Export[FileNameJoin[{imgDir, "coevolution_genome_vs_noise.png"}],
  ListLinePlot[
    Table[Transpose[{sweep, Table[finalGenomes[eps][[i]], {eps, sweep}]}], {i, 5}],
    PlotStyle -> pcols, PlotMarkers -> Automatic, PlotRange -> {0, 1},
    PlotLegends -> paramNames, Frame -> True,
    FrameLabel -> {"execution noise \[Epsilon]", "co-evolved mean genome"},
    PlotLabel -> "Co-evolved population genome vs noise", ImageSize -> 760],
  ImageResolution -> 144];

Print["[coevo] done. final mean genome low vs high noise:"];
Print["   eps=0   : ", Thread[paramNames -> Round[resLow["finalGenome"], 0.01]]];
Print["   eps=0.15: ", Thread[paramNames -> Round[resHigh["finalGenome"], 0.01]]];
