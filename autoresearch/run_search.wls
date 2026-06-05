#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  run_search.wls -- drives the autoresearch propose -> evaluate -> keep
    loop over a curated sequence of hypotheses (the "researcher's notebook").
    Uses the fixed evaluation protocol below. Records every experiment to
    results.tsv and saves any strategy that beats the running champion into
    champions/. This is the autoresearch loop, batched into one kernel so the
    18x6x8x150 evaluations don't pay repeated startup cost.

    Each hypothesis is a pure function candidate[myHist,oppHist] -> 0|1.      *)

here     = DirectoryName[$InputFileName];
repoRoot = ParentDirectory[here];
Get[FileNameJoin[{repoRoot, "wolfram", "axelrod.wl"}]];
Get[FileNameJoin[{repoRoot, "wolfram", "strategies_ext.wl"}]];

(* fixed evaluation protocol -- never edited to win *)
rounds = 150; reps = 8; evalSeed = 20260529;
sweep  = {0., 0.02, 0.05, 0.1, 0.15, 0.2};

(* opponent field = classics + modern (champions excluded during search so
   the metric is a stable target; champions are added to the final arena) *)
field = Join[
  Association@Table[n -> Axelrod`strategies[n], {n, Axelrod`strategyNames}],
  StrategiesExt`extendedStrategies];
opps = Values[field];

fieldScore[s_, eps_] := BlockRandom[SeedRandom[evalSeed];
  Mean@Table[Axelrod`meanMatch[s, opp, rounds, eps, reps][[1]], {opp, opps}]];
sweepScore[s_] := Mean@Table[fieldScore[s, eps], {eps, sweep}];

tft = Axelrod`strategies["TitForTat"];
tftScore = sweepScore[tft];
Print["[search] TFT baseline = ", NumberForm[tftScore, {6, 4}],
  "  (field of ", Length[opps], ")"];

(* ===================================================================== *)
(* The researcher's hypotheses (propose).  Each: {name, description, fn}. *)
(* ===================================================================== *)
gtft[p_] := Function[{me, opp}, If[opp === {}, 1,
  If[Last[opp] == 1, 1, If[RandomReal[] < p, 1, 0]]]];

(* contrite: don't punish a defection that followed our own defection *)
contrite[me_, opp_] := If[opp === {}, 1,
  If[Last[opp] == 0 && me =!= {} && Last[me] == 1, 0, 1]];

(* noise-estimating generous TFT: forgive with prob tied to observed
   "surprise" rate (how often opp's move flipped between consecutive rounds) *)
adaptiveGen[me_, opp_] := If[opp === {}, 1,
  If[Last[opp] == 1, 1,
    Module[{flips},
      flips = If[Length[opp] >= 2,
        N@Mean@Table[Boole[opp[[i]] != opp[[i - 1]]], {i, 2, Length[opp]}], 0.];
      If[RandomReal[] < Clip[2 flips, {0, 0.5}], 1, 0]]]];

(* contrite + generous: contrition first, then forgive residual defections *)
contriteGen[p_] := Function[{me, opp}, If[opp === {}, 1,
  Which[
    Last[opp] == 1, 1,
    me =!= {} && Last[me] == 0, 1,             (* contrition: I defected last *)
    RandomReal[] < p, 1,                        (* generosity *)
    True, 0]]];

(* Pavlov with a defection guard against pure cooperators is unnecessary;
   try plain WSLS and a "firm but fair" = WSLS that defects on S only *)
wsls[me_, opp_] := If[me === {}, 1,
  If[Last[me] == Last[opp], 1, 0]];

(* tit-for-2-tats: only retaliate after two consecutive defections *)
tf2t[me_, opp_] := If[Length[opp] >= 2 && opp[[-1]] == 0 && opp[[-2]] == 0, 0, 1];

(* contrite tit-for-2-tats: forgiving AND contrite *)
contriteTF2T[me_, opp_] := Which[
  Length[opp] < 2, 1,
  opp[[-1]] == 0 && opp[[-2]] == 0 && ! (me =!= {} && Last[me] == 0), 0,
  True, 1];

(* "Gradual generosity": TF2T core but with a small forgiveness leak *)
gen2[p_] := Function[{me, opp},
  Which[
    Length[opp] < 2, 1,
    opp[[-1]] == 0 && opp[[-2]] == 0, If[RandomReal[] < p, 1, 0],
    True, 1]];

hypotheses = {
  {"GenerousTFT-0.1",  "GTFT, forgive 10% of defections",          gtft[0.1]},
  {"GenerousTFT-0.2",  "GTFT, forgive 20%",                        gtft[0.2]},
  {"GenerousTFT-0.3",  "GTFT, forgive 30%",                        gtft[0.3]},
  {"Contrite",         "don't punish defection after own defect",  contrite},
  {"AdaptiveGenerous", "forgiveness scaled to observed flip rate",  adaptiveGen},
  {"ContriteGen-0.1",  "contrition + 10% generosity",              contriteGen[0.1]},
  {"ContriteGen-0.2",  "contrition + 20% generosity",              contriteGen[0.2]},
  {"WSLS",             "win-stay lose-shift (Pavlov)",             wsls},
  {"TitFor2Tats",      "retaliate only after 2 defections",        tf2t},
  {"ContriteTF2T",     "TF2T + contrition",                        contriteTF2T},
  {"GenerousTF2T-0.3", "TF2T with 30% forgiveness leak",           gen2[0.3]}
};

(* ===================================================================== *)
(* The loop (evaluate -> keep/discard).                                   *)
(* ===================================================================== *)
champDir = FileNameJoin[{here, "champions"}];
If[! DirectoryQ[champDir], CreateDirectory[champDir]];

resultsFile = FileNameJoin[{here, "results.tsv"}];
rd[x_] := N@Round[x, 0.0001];   (* plain numbers serialise to TSV (NumberForm does not) *)
log = {{"iter", "score", "tft_baseline", "status", "description"}};
log = Append[log, {0, rd[tftScore], rd[tftScore], "init", "Tit-for-Tat baseline"}];

champScore = tftScore;
champions = {};   (* {name, score, fn} that beat the baseline *)

(* serialisable source for each champion strategy (so champions/*.wl is real) *)
champSource = <|
  "GenerousTFT-0.1" -> "candidate[me_,opp_]:=If[opp==={},1,If[Last[opp]==1,1,If[RandomReal[]<0.1,1,0]]];",
  "GenerousTFT-0.2" -> "candidate[me_,opp_]:=If[opp==={},1,If[Last[opp]==1,1,If[RandomReal[]<0.2,1,0]]];",
  "GenerousTFT-0.3" -> "candidate[me_,opp_]:=If[opp==={},1,If[Last[opp]==1,1,If[RandomReal[]<0.3,1,0]]];",
  "Contrite" -> "candidate[me_,opp_]:=If[opp==={},1,If[Last[opp]==0&&me=!={}&&Last[me]==1,0,1]];",
  "AdaptiveGenerous" -> "candidate[me_,opp_]:=If[opp==={},1,If[Last[opp]==1,1,Module[{flips=If[Length[opp]>=2,N@Mean@Table[Boole[opp[[i]]!=opp[[i-1]]],{i,2,Length[opp]}],0.]},If[RandomReal[]<Clip[2 flips,{0,0.5}],1,0]]]];",
  "ContriteGen-0.1" -> "candidate[me_,opp_]:=If[opp==={},1,Which[Last[opp]==1,1,me=!={}&&Last[me]==0,1,RandomReal[]<0.1,1,True,0]];",
  "ContriteGen-0.2" -> "candidate[me_,opp_]:=If[opp==={},1,Which[Last[opp]==1,1,me=!={}&&Last[me]==0,1,RandomReal[]<0.2,1,True,0]];",
  "WSLS" -> "candidate[me_,opp_]:=If[me==={},1,If[Last[me]==Last[opp],1,0]];",
  "TitFor2Tats" -> "candidate[me_,opp_]:=If[Length[opp]>=2&&opp[[-1]]==0&&opp[[-2]]==0,0,1];",
  "ContriteTF2T" -> "candidate[me_,opp_]:=Which[Length[opp]<2,1,opp[[-1]]==0&&opp[[-2]]==0&&!(me=!={}&&Last[me]==0),0,True,1];",
  "GenerousTF2T-0.3" -> "candidate[me_,opp_]:=Which[Length[opp]<2,1,opp[[-1]]==0&&opp[[-2]]==0,If[RandomReal[]<0.3,1,0],True,1];"
|>;

iter = 0;
Do[
  Module[{name = h[[1]], desc = h[[2]], fn = h[[3]], sc, status},
    iter++;
    sc = sweepScore[fn];
    status = If[sc > champScore, "keep", "discard"];
    If[sc > champScore, champScore = sc];
    If[sc > tftScore,
      AppendTo[champions, {name, sc, fn}];
      Export[FileNameJoin[{champDir, name <> ".wl"}],
        "(* champion: " <> desc <> "  (sweep score " <>
         ToString[NumberForm[sc, {6, 4}]] <> ") *)\n" <> champSource[name],
        "Text"]];
    AppendTo[log, {iter, rd[sc], rd[tftScore], status, desc}];
    Print["[search] ", StringPadRight[name, 18],
      " score=", NumberForm[sc, {6, 4}],
      "  vs TFT ", NumberForm[tftScore, {6, 4}],
      "  ", If[sc > tftScore, "(+"<>ToString[NumberForm[sc-tftScore,{5,4}]]<>")", "(-)"],
      "  ", status];
  ],
  {h, hypotheses}];

Export[resultsFile, log, "TSV"];

(* report the top strategies *)
ranked = Reverse@SortBy[champions, #[[2]] &];
Print["\n[search] champions that beat TFT (", Length[ranked], "):"];
Do[Print["   ", StringPadRight[c[[1]], 18], NumberForm[c[[2]], {6, 4}]], {c, ranked}];
Print["[search] best = ", If[ranked === {}, "none", ranked[[1, 1]]],
  "  saved ", Length[FileNames["*.wl", champDir]], " champion files."];
