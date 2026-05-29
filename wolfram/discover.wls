#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  discover.wls -- Phase 4: automated strategy discovery.

    In the spirit of an automated-research loop (propose -> evaluate ->
    accept -> repeat; cf. Karpathy's "autoresearch"), we search the space
    of memory-one strategies theta = (open, pCC, pCD, pDC, pDD) in [0,1]^5
    for the best response to the standard field, AT A GIVEN NOISE LEVEL.

    Two searchers:
      * hillClimb  -- greedy accept/reject loop, logged to a TSV exactly
                      like autoresearch's results.tsv (the narrative).
      * geneticSearch -- population-based search (the workhorse), used to
                      sweep noise and show the optimal strategy SHIFTS from
                      reciprocal (TFT-like) to forgiving (GTFT/Pavlov-like)
                      as noise rises -- and beats plain Tit-for-Tat.

    The objective is made deterministic per theta with BlockRandom so the
    search is reproducible.                                              *)

Get[FileNameJoin[{DirectoryName[$InputFileName], "axelrod.wl"}]];
repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
dataDir  = FileNameJoin[{repoRoot, "data"}];
imgDir   = FileNameJoin[{repoRoot, "docs", "images"}];
Scan[If[! DirectoryQ[#], CreateDirectory[#, CreateIntermediateDirectories -> True]] &,
  {dataDir, imgDir}];

SeedRandom[7];
names = Axelrod`strategyNames;
field = Axelrod`strategies /@ names;     (* opponent pool *)

(* ---- parameters (modest, for a demonstration; scale up to refine) -- *)
rounds = 120;
reps   = 4;
evalSeed = 12345;

(* candidate from parameter vector *)
candidate[theta_] := Axelrod`memOne[theta[[1]], theta[[2 ;;]]];

(* mean score of a candidate against the whole field at noise eps,
   deterministic per theta via a fixed random seed *)
fitness[theta_, eps_] := BlockRandom[
  SeedRandom[evalSeed];
  Mean @ Table[
    Axelrod`meanMatch[candidate[theta], opp, rounds, eps, reps][[1]], {opp, field}]
];

(* TFT baseline field score at noise eps (same protocol) *)
tftTheta = {1, 1, 0, 1, 0};
tftScore[eps_] := fitness[tftTheta, eps];

clip[v_] := Clip[v, {0, 1}];

(* ---- greedy hill-climb (the autoresearch loop) -------------------- *)
hillClimb[eps_, iters_, sigma_] := Module[
  {champ = tftTheta, champF, prop, propF, log = {}},
  champF = fitness[champ, eps];
  log = {{0, champ, champF, "init"}};
  Do[
    prop  = clip[champ + RandomVariate[NormalDistribution[0, sigma], 5]];
    propF = fitness[prop, eps];
    If[propF > champF,
      champ = prop; champF = propF;
      AppendTo[log, {k, prop, propF, "keep"}],
      AppendTo[log, {k, prop, propF, "discard"}]],
    {k, iters}];
  <|"champ" -> champ, "fit" -> champF, "log" -> log|>
];

(* ---- genetic search ----------------------------------------------- *)
geneticSearch[eps_, popSize_, gens_, sigma_] := Module[
  {pop, fits, best, bestF, hist = {}, ranked, elite, children, parents},
  pop = Table[RandomReal[{0, 1}, 5], {popSize}];
  pop[[1]] = tftTheta;                       (* seed with TFT *)
  Do[
    fits   = fitness[#, eps] & /@ pop;
    ranked = Reverse @ Ordering[N@fits];
    pop    = pop[[ranked]]; fits = fits[[ranked]];
    best   = pop[[1]]; bestF = fits[[1]];
    AppendTo[hist, bestF];
    elite  = pop[[;; Max[2, Round[popSize/5]]]];
    children = Table[
      parents = RandomChoice[elite, 2];
      clip[ MapThread[If[RandomReal[] < 0.5, #1, #2] &, parents]
            + RandomVariate[NormalDistribution[0, sigma], 5] ],
      {popSize - Length[elite]}];
    pop = Join[elite, children],
    {gens}];
  <|"champ" -> best, "fit" -> bestF, "hist" -> hist|>
];

(* ---- narrative: hill-climb at low vs high noise -------------------- *)
Print["[discover] hill-climb at eps=0 and eps=0.1 ..."];
hcLow  = hillClimb[0.,  150, 0.12];
hcHigh = hillClimb[0.1, 150, 0.12];

(* write RAW numbers (NumberForm objects do not serialise to TSV) *)
writeLog[file_, hc_] := Export[FileNameJoin[{dataDir, file}],
  Prepend[
    {#[[1]], N[#[[3]]], Sequence @@ N[#[[2]]], #[[4]]} & /@ hc["log"],
    {"iter", "fitness", "open", "pCC", "pCD", "pDC", "pDD", "status"}],
  "TSV"];
writeLog["discovery_log_eps0.tsv",  hcLow];
writeLog["discovery_log_eps0.1.tsv", hcHigh];

(* ---- workhorse: genetic search swept over noise ------------------- *)
sweep = {0., 0.05, 0.1, 0.15, 0.2};
Print["[discover] genetic search over noise sweep ", sweep, " ..."];
gsResults = Association @ Table[
  Module[{t0 = AbsoluteTime[], g},
    g = geneticSearch[eps, 20, 14, 0.10];
    Print["   eps=", NumberForm[eps, {4, 3}],
      "  champ=", NumberForm[g["champ"], {4, 3}],
      "  fit=", NumberForm[g["fit"], {5, 3}],
      "  TFT=", NumberForm[tftScore[eps], {5, 3}],
      "  (", Round[AbsoluteTime[] - t0, 0.1], "s)"];
    eps -> g],
  {eps, sweep}];

(* ---- save discovered strategies ----------------------------------- *)
Export[FileNameJoin[{dataDir, "discovered_strategies.csv"}],
  Join[
    {{"noise", "open", "pCC", "pCD", "pDC", "pDD", "fieldScore", "tftFieldScore", "improvement"}},
    Table[
      Join[{eps}, gsResults[eps]["champ"],
        {gsResults[eps]["fit"], tftScore[eps],
         gsResults[eps]["fit"] - tftScore[eps]}],
      {eps, sweep}]]];

(* Figures are rendered by wolfram/figures.wls from the CSV/TSV outputs. *)
Print["[discover] done."];
Print["  eps=0   champ -> ", Thread[paramNames -> Round[gsResults[0.]["champ"], 0.01]]];
Print["  eps=0.2 champ -> ", Thread[paramNames -> Round[gsResults[0.2]["champ"], 0.01]]];
