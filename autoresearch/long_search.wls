#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ===================================================================== *)
(* long_search.wls -- the autoresearch loop, scaled up to run autonomously *)
(* for a wall-clock budget (hours).                                        *)
(*                                                                         *)
(* This is the same propose -> evaluate -> keep idea as Karpathy's          *)
(* autoresearch, but the "experiment" is a Prisoner's-Dilemma strategy and *)
(* the search runs natively in the Wolfram Language with no LLM / GPU /     *)
(* API. To make 8h of compute productive (not converge-then-idle) it uses: *)
(*   - a RICH strategy space: memory-two stochastic strategies             *)
(*     theta = (open, p1..p16) in [0,1]^17, which strictly contains         *)
(*     memory-one (TFT, Pavlov, GTFT, ...).                                 *)
(*   - an ISLAND genetic algorithm (several sub-populations) with ring      *)
(*     migration and soft-restart on stagnation, to keep exploring.        *)
(*   - a fixed, deterministic fitness (mean field score over a noise sweep) *)
(*     so improvements are comparable across the whole run.                 *)
(*   - a HALL OF FAME of the best distinct strategies ever seen.            *)
(*   - frequent disk CHECKPOINTS so progress is never lost.                 *)
(*                                                                         *)
(* Usage:  wolframscript -file long_search.wls [budgetSeconds]             *)
(*         (default budget = 28800 s = 8 h)                                 *)
(* ===================================================================== *)

here     = DirectoryName[$InputFileName];
repoRoot = ParentDirectory[here];
Get[FileNameJoin[{repoRoot, "wolfram", "axelrod.wl"}]];
Get[FileNameJoin[{repoRoot, "wolfram", "strategies_ext.wl"}]];

outDir = FileNameJoin[{here, "long"}];
If[! DirectoryQ[outDir], CreateDirectory[outDir, CreateIntermediateDirectories -> True]];

budget = If[Length[$ScriptCommandLine] >= 2 && $ScriptCommandLine[[2]] =!= "",
   ToExpression[$ScriptCommandLine[[2]]], 28800];

(* ---- fixed evaluation protocol ------------------------------------- *)
(* search-phase eval: deterministic (fixed seed) so 3 reps is reproducible and
   enough to rank candidates -- noise is already averaged over 6 levels x 18
   opponents. The hall-of-fame is re-scored at high rep with fresh seeds at the
   end, which is where the honest numbers come from. Profiled ~0.65 s/genome. *)
rounds   = 150;
reps     = 3;
evalSeed = 20260530;
noiseSweep = {0., 0.02, 0.05, 0.1, 0.15, 0.2};

(* fixed opponent field: 13 classic + 5 modern = 18 strategies *)
field = Join[
  Axelrod`strategies /@ Axelrod`strategyNames,
  Values[StrategiesExt`extendedStrategies]];
nField = Length[field];

(* ---- memory-two strategy ------------------------------------------ *)
(* genome = {open, p1..p16}; state index from (m1,o1,m2,o2), m1/o1 = most
   recent. Histories padded with cooperation before the game starts.       *)
stateIdx[m1_, o1_, m2_, o2_] := 1 + 8 (1 - m1) + 4 (1 - o1) + 2 (1 - m2) + (1 - o2);

(* compute the cooperate-probability for the current state, then sample once.
   (No Return[] -- inside Function+Module it does not escape cleanly and leaks
   an unevaluated Return[...] head.) *)
mem2[genome_] := With[{op = genome[[1]], p = genome[[2 ;; 17]]},
  Function[{me, opp},
   Module[{n = Length[opp], pc},
    pc = If[n == 0, op,
      p[[ stateIdx[Last[me], Last[opp],
           If[n == 1, 1, me[[-2]]], If[n == 1, 1, opp[[-2]]]] ]] ];
    If[RandomReal[] < pc, 1, 0]]]];

(* deterministic fitness: mean field score at noise eps, fixed seed *)
fitAt[genome_, eps_] := BlockRandom[SeedRandom[evalSeed];
  Module[{s = mem2[genome]},
   Mean @ Table[Axelrod`meanMatch[s, opp, rounds, eps, reps][[1]], {opp, field}]]];
sweepFit[genome_] := Mean @ Table[fitAt[genome, eps], {eps, noiseSweep}];

genomeFromRule[open_, rule_] := Module[{p = ConstantArray[0., 16]},
  Do[With[{idx = stateIdx[m1, o1, m2, o2]}, p[[idx]] = N[rule[m1, o1, m2, o2]]],
   {m1, {1, 0}}, {o1, {1, 0}}, {m2, {1, 0}}, {o2, {1, 0}}];
  Prepend[p, N[open]]];

tftGenome  = genomeFromRule[1, Function[{m1, o1, m2, o2}, o1]];
gtftGenome = genomeFromRule[1, Function[{m1, o1, m2, o2}, If[o1 == 1, 1, 1/3]]];
pavGenome  = genomeFromRule[1, Function[{m1, o1, m2, o2}, Boole[m1 == o1]]];
tf2tGenome = genomeFromRule[1, Function[{m1, o1, m2, o2}, If[o1 == 0 && o2 == 0, 0, 1]]];
allcGenome = genomeFromRule[1, Function[{m1, o1, m2, o2}, 1]];
seeds = {tftGenome, gtftGenome, pavGenome, tf2tGenome, allcGenome};

tftSweep = sweepFit[tftGenome];

(* ---- GA operators ------------------------------------------------- *)
clip[g_] := Clip[g, {0, 1}];
mutate[g_, sigma_] := clip[g + RandomVariate[NormalDistribution[0, sigma], 17]];
crossover[g1_, g2_] := MapThread[If[RandomReal[] < 0.5, #1, #2] &, {g1, g2}];
tournamentSelect[pop_, k_] := (* pop = sorted {{g,fit}..} desc *)
  pop[[ Min @ RandomSample[Range[Length[pop]], k] ]][[1]];

(* ---- island GA parameters ----------------------------------------- *)
nIslands    = 4;
islandPop   = 16;
nElite      = 3;
baseSigma   = 0.08;
migrateEvery= 15;
patience    = 40;
hofSize     = 14;
ckptEvery   = 5;     (* progress.tsv row every N gens (fast pace now) *)
hofEvery    = 25;    (* dump hall of fame + best.wl + status every N gens *)

(* island = list of {genome, fitOrNull}; evaluate Null fits *)
evalIsland[isl_] := (If[#[[2]] === Null, {#[[1]], sweepFit[#[[1]]]}, #] & /@ isl);
sortIsl[isl_]    := Reverse@SortBy[isl, #[[2]] &];

initIsland[seedQ_] := Module[{base},
  base = Table[{RandomReal[{0, 1}, 17], Null}, {islandPop}];
  If[seedQ, MapIndexed[(base[[ First[#2] ]] = {#1, Null}) &, seeds]];
  base];

(* hall of fame: list of {genome, fit}; dedup by rounded genome *)
hofKey[g_] := Round[g, 0.05];
updateHOF[hof_, cands_] := Module[{all, ded},
  all = Join[hof, cands];
  ded = Values @ GroupBy[all, hofKey[#[[1]]] &, First @ Reverse@SortBy[#, Last] &];
  Take[Reverse@SortBy[ded, Last], Min[hofSize, Length[ded]]]];

(* serialise a memory-two genome to a champion candidate file *)
mem2Source[genome_] := Module[{op, p},
  op = N@Round[genome[[1]], 0.0001];
  p  = N@Round[genome[[2 ;; 17]], 0.0001];
  "(* discovered memory-two strategy (long_search) *)\n" <>
  "candidate[me_, opp_] := Module[{n = Length[opp], m1, o1, m2, o2, " <>
  "p = " <> ToString[p, InputForm] <> ", op = " <> ToString[op, InputForm] <> "},\n" <>
  "  If[n == 0, Return[If[RandomReal[] < op, 1, 0]]];\n" <>
  "  m1 = Last[me]; o1 = Last[opp];\n" <>
  "  If[n == 1, m2 = 1; o2 = 1, m2 = me[[-2]]; o2 = opp[[-2]]];\n" <>
  "  If[RandomReal[] < p[[1 + 8 (1-m1) + 4 (1-o1) + 2 (1-m2) + (1-o2)]], 1, 0]]\n"];

(* ---- init --------------------------------------------------------- *)
SeedRandom[77];
islands = Table[sortIsl@evalIsland@initIsland[isl == 1], {isl, nIslands}];
stagn   = ConstantArray[0, nIslands];
hof     = updateHOF[{}, Flatten[islands, 1]];
gBest   = hof[[1]];

progressFile = FileNameJoin[{outDir, "progress.tsv"}];
Export[progressFile,
  {{"gen", "elapsed_min", "best_sweep", "tft_sweep", "improvement",
    "hof_size", "islands_restarted"}}, "TSV"];

t0 = AbsoluteTime[];
Print["[long] budget=", budget, "s  field=", nField,
  "  TFT sweep baseline=", NumberForm[tftSweep, {6, 4}]];
Print["[long] initial best sweep=", NumberForm[gBest[[2]], {6, 4}]];

logProgress[gen_, restarts_] := Module[{row},
  row = {gen, N@Round[(AbsoluteTime[] - t0)/60., 0.01],
     N@Round[gBest[[2]], 0.000001], N@Round[tftSweep, 0.000001],
     N@Round[gBest[[2]] - tftSweep, 0.000001], Length[hof], restarts};
  (* append by re-reading + writing (small file) *)
  Module[{cur = Import[progressFile, "TSV"]},
   Export[progressFile, Append[cur, row], "TSV"]];
];

dumpHOF[gen_] := Module[{rows},
  rows = Table[
    Join[{i, N@Round[hof[[i, 2]], 0.000001]},
      N@Round[hof[[i, 1]], 0.0001]], {i, Length[hof]}];
  Export[FileNameJoin[{outDir, "hall_of_fame.tsv"}],
    Join[{Join[{"rank", "sweep_fit"},
        Prepend[Table["p" <> ToString[k], {k, 16}], "open"]]}, rows], "TSV"];
  Export[FileNameJoin[{outDir, "best_strategy.wl"}], mem2Source[gBest[[1]]], "Text"];
  Export[FileNameJoin[{outDir, "status.txt"}],
    "gen=" <> ToString[gen] <> "  elapsed_min=" <>
    ToString[N@Round[(AbsoluteTime[] - t0)/60., 0.1]] <> "  best_sweep=" <>
    ToString[N@Round[gBest[[2]], 0.0001]] <> "  tft=" <>
    ToString[N@Round[tftSweep, 0.0001]] <> "  (+" <>
    ToString[N@Round[gBest[[2]] - tftSweep, 0.0001]] <> ")", "Text"];
];

(* ---- main loop: run until budget exhausted ------------------------ *)
gen = 0;
While[AbsoluteTime[] - t0 < budget,
  gen++;
  restartsThisGen = 0;
  Do[
    Module[{isl = islands[[k]], elites, children, sigma, prevBest},
     prevBest = isl[[1, 2]];
     (* stagnation -> soft restart (keep best, randomise rest, bigger sigma) *)
     If[stagn[[k]] >= patience,
      restartsThisGen++;
      elites = {isl[[1]]};
      children = Table[{mutate[isl[[1, 1]], 0.25], Null}, {islandPop - 1}];
      islands[[k]] = sortIsl@evalIsland@Join[elites, children];
      stagn[[k]] = 0,
      (* normal generation *)
      sigma = baseSigma;
      elites = Take[isl, nElite];
      children = Table[
        {mutate[crossover[tournamentSelect[isl, 3], tournamentSelect[isl, 3]], sigma], Null},
        {islandPop - nElite}];
      islands[[k]] = sortIsl@evalIsland@Join[elites, children];
     ];
     If[islands[[k]][[1, 2]] > prevBest + 10.^-7, stagn[[k]] = 0, stagn[[k]]++];
    ],
    {k, nIslands}];

  (* ring migration: best of island k replaces worst of island k+1 *)
  If[Mod[gen, migrateEvery] == 0,
   Module[{bests = islands[[All, 1]]},
    Do[islands[[Mod[k, nIslands] + 1, -1]] = bests[[k]], {k, nIslands}];
    islands = sortIsl /@ islands]];

  hof   = updateHOF[hof, islands[[All, 1]]];
  gBest = hof[[1]];

  If[Mod[gen, ckptEvery] == 0, logProgress[gen, restartsThisGen]];
  If[Mod[gen, hofEvery] == 0,
   dumpHOF[gen];
   Print["[long] gen=", gen, "  t=", NumberForm[(AbsoluteTime[]-t0)/60., {5,1}],
     "min  best=", NumberForm[gBest[[2]], {6, 4}],
     "  (+", NumberForm[gBest[[2]] - tftSweep, {5, 4}], ")  hof=", Length[hof]]];
];

dumpHOF[gen];
Print["[long] search loop done at gen=", gen,
  "  (", NumberForm[(AbsoluteTime[]-t0)/60., {5,1}], " min)"];

(* ===================================================================== *)
(* FINAL high-rep, fresh-seed re-evaluation of the hall of fame to get    *)
(* honest numbers and filter overfit champions.                          *)
(* ===================================================================== *)
finalRounds = 150; finalReps = 20; finalSeeds = {101, 202, 303};
robustFitAt[genome_, eps_] := Mean @ Table[
  BlockRandom[SeedRandom[sd];
   Mean @ Table[Axelrod`meanMatch[mem2[genome], opp, finalRounds, eps, finalReps][[1]],
     {opp, field}]],
  {sd, finalSeeds}];
robustSweep[genome_] := Mean @ Table[robustFitAt[genome, eps], {eps, noiseSweep}];

tftRobust = robustSweep[tftGenome];
Print["[long] final robust re-eval of ", Length[hof], " hall-of-fame strategies ..."];
finalRows = Table[
  Module[{g = hof[[i, 1]], rs, perNoise},
   rs = robustSweep[g];
   perNoise = Table[N@Round[robustFitAt[g, eps], 0.0001], {eps, noiseSweep}];
   Print["   #", i, "  searchFit=", NumberForm[hof[[i, 2]], {6, 4}],
     "  robustSweep=", NumberForm[rs, {6, 4}], "  vsTFT=", NumberForm[rs - tftRobust, {6, 4}]];
   Join[{i, N@Round[hof[[i, 2]], 0.0001], N@Round[rs, 0.0001],
      N@Round[rs - tftRobust, 0.0001]}, perNoise]],
  {i, Length[hof]}];

Export[FileNameJoin[{outDir, "final_report.tsv"}],
  Join[{Join[{"rank", "search_fit", "robust_sweep", "vs_tft"},
      ("eps_" <> ToString[#] &) /@ noiseSweep]}, finalRows], "TSV"];

(* save the robustly-best strategy as the headline champion *)
bestRobustIdx = First@Ordering[-finalRows[[All, 3]]];
bestRobustGenome = hof[[bestRobustIdx, 1]];
Export[FileNameJoin[{outDir, "champion_robust.wl"}], mem2Source[bestRobustGenome], "Text"];
Export[FileNameJoin[{outDir, "champion_robust_meta.txt"}],
  "robust_sweep=" <> ToString[N@Round[robustSweep[bestRobustGenome], 0.0001]] <>
  "  tft_robust=" <> ToString[N@Round[tftRobust, 0.0001]] <>
  "  improvement=" <> ToString[N@Round[robustSweep[bestRobustGenome] - tftRobust, 0.0001]] <>
  "  genome=" <> ToString[N@Round[bestRobustGenome, 0.0001], InputForm], "Text"];

Print["[long] FINISHED. robust champion vsTFT=+",
  NumberForm[robustSweep[bestRobustGenome] - tftRobust, {6, 4}],
  "  generations=", gen];

(* exit cleanly so the kernel never lingers as a runaway *)
Exit[0];
