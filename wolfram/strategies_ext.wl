(* ::Package:: *)

(* ===================================================================== *)
(* strategies_ext.wl -- richer strategy representations beyond memory-one *)
(*   - fsmStrategy : finite-state (Mealy) machines                        *)
(*   - memNStrategy: deterministic memory-n lookup strategies             *)
(*   - named human-designed "modern" strategies that need full history    *)
(*     (Contrite TFT, Omega-TFT, Adaptive, Remorse, ...)                  *)
(*                                                                        *)
(* Loaded after axelrod.wl. All builders return a pure function           *)
(* s[myHist, oppHist] -> 0|1 compatible with the core engine.            *)
(* ===================================================================== *)

BeginPackage["StrategiesExt`", {"Axelrod`"}];

ClearAll["StrategiesExt`*", "StrategiesExt`Private`*"];

fsmStrategy::usage   = "fsmStrategy[{initAction, table}] builds a Mealy-machine strategy. table is a length-2k integer-coded transition list; see randomFSM.";
randomFSM::usage     = "randomFSM[k] returns a random k-state machine spec {initAction, table}.";
fsmArity::usage      = "fsmArity[k] = number of integer genes in a k-state machine spec.";
fsmFromGenes::usage  = "fsmFromGenes[k, genes] decodes an integer gene vector into an fsmStrategy.";
fsmDecode::usage     = "fsmDecode[k, genes] -> {initAction, table} (the decoded machine spec).";
mutateFSM::usage     = "mutateFSM[k, genes, rate] returns a mutated gene vector.";
crossoverFSM::usage  = "crossoverFSM[g1, g2] uniform crossover of two gene vectors.";
fsmComplexity::usage = "fsmComplexity[k, genes] = number of reachable states.";
extendedStrategies::usage = "extendedStrategies: Association of named full-history strategies.";

Begin["`Private`"];

(* ----------------------------------------------------------------- *)
(* Mealy machine.  k states (1..k).  Genome integer-coded:            *)
(*   initAction in {0,1}                                              *)
(*   for each state s and each observed opponent-last in {C=1, D=0}:  *)
(*       action in {0,1},  nextState in {1..k}                        *)
(* On the opening move there is no opponent-last -> use initAction    *)
(* and start in state 1. Thereafter we replay the opponent history.   *)
(* ----------------------------------------------------------------- *)

fsmArity[k_] := 1 + 4 k;   (* initAction + k states * (oppC:{act,next}, oppD:{act,next}) *)

(* clip helpers for decoding *)
bit[x_]      := Mod[Round[x], 2];
stateOf[x_, k_] := 1 + Mod[Round[x] - 1, k];

(* decode a gene vector into the spec {initAction, table}             *)
fsmDecode[k_Integer, genes_List] := Module[{initA, tbl},
  initA = bit[genes[[1]]];
  (* table[[s]] = {{actC,nextC},{actD,nextD}} *)
  tbl = Table[
    With[{base = 1 + (s - 1)*4},
     {{bit[genes[[base + 1]]], stateOf[genes[[base + 2]], k]},
      {bit[genes[[base + 3]]], stateOf[genes[[base + 4]], k]}}],
    {s, k}];
  {initA, tbl}
];

fsmFromGenes[k_Integer, genes_List] := fsmStrategy[fsmDecode[k, genes]];

fsmStrategy[{initA_, tbl_}] := Function[{me, opp},
  Module[{s = 1, a = initA, o},
    Do[
      o = opp[[t]];                        (* opponent's actual last move *)
      With[{rule = tbl[[s, If[o == 1, 1, 2]]]},
        a = rule[[1]]; s = rule[[2]]],
      {t, Length[opp]}];
    (* a now holds the action AFTER processing the whole history,
       i.e. the move to make now; on the empty history a = initA *)
    a
  ]
];

randomFSM[k_Integer] := Module[{},
  Join[{RandomInteger[1]},
   Flatten@Table[
     {RandomInteger[1], RandomInteger[{1, k}],
      RandomInteger[1], RandomInteger[{1, k}]}, {k}]]
];

mutateFSM[k_Integer, genes_List, rate_] := MapIndexed[
  Function[{g, idx},
    If[RandomReal[] < rate,
     If[idx[[1]] == 1 || Mod[idx[[1]] - 1, 2] == 1,
      RandomInteger[1],                 (* action / initAction gene *)
      RandomInteger[{1, k}]],            (* next-state gene *)
     g]],
  genes];

crossoverFSM[g1_List, g2_List] := MapThread[
  If[RandomReal[] < 0.5, #1, #2] &, {g1, g2}];

(* reachable-state count = a simple complexity measure.
   Counts states reachable from state 1 following both C and D edges. *)
fsmComplexity[k_Integer, genes_List] := Module[
  {tbl, seen = {1}, frontier = {1}, nxt},
  tbl = fsmDecode[k, genes][[2]];        (* the transition table *)
  While[frontier =!= {},
    nxt = DeleteDuplicates @ Flatten[
       (Function[s, {tbl[[s, 1, 2]], tbl[[s, 2, 2]]}] /@ frontier)];
    nxt = Complement[nxt, seen];
    seen = Union[seen, nxt]; frontier = nxt];
  Length[seen]
];

(* ----------------------------------------------------------------- *)
(* Human-designed full-history "modern" strategies                    *)
(* ----------------------------------------------------------------- *)

(* Contrite TFT (approx): like TFT, but forgives an opponent defection
   that plausibly followed our OWN earlier (noise-induced) defection,
   i.e. don't retaliate if we just defected. Reduces echo with TFT-likes
   under noise. *)
contriteTFT[me_, opp_] := If[opp === {}, 1,
  If[Last[opp] == 0 && (me =!= {} && Last[me] == 1), 0, 1]];

(* Remorseful Pavlov: WSLS but apologises (cooperates) after a CD outcome
   it likely caused. *)
remorse[me_, opp_] := If[me === {}, 1,
  Module[{ml = Last[me], ol = Last[opp]},
   Which[ml == 1 && ol == 1, 1, ml == 0 && ol == 0, 1,
         ml == 0 && ol == 1, 0, True, 1]]];

(* Adaptive: cooperate unless the opponent's running defection rate is
   high; tolerance widens with apparent noise (own-vs-expected mismatch). *)
adaptive[me_, opp_] := If[Length[opp] < 3, 1,
  Module[{dr = N@Mean[1 - opp]},
   If[dr > 0.5, 0, 1]]];

(* Omega-TFT (simplified Slany-Kienreich): track a deadlock counter and a
   randomness counter; break C/D deadlocks by cooperating, punish apparent
   randomness by defecting. *)
omegaTFT[me_, opp_] := Module[
  {n = Length[opp], deadlock = 0, randomness = 0, prevO, action = 1},
  If[n == 0, Return[1]];
  Do[
   prevO = opp[[t]];
   action = prevO;                       (* TFT core *)
   If[t >= 2,
    If[opp[[t]] != opp[[t - 1]], randomness++, randomness = Max[0, randomness - 1]];
    If[t >= 2 && me =!= {} && t <= Length[me] && opp[[t]] != If[t-1>=1, opp[[t-1]],1],
       deadlock++]];
   , {t, n}];
  Which[
   randomness >= 8, 0,                   (* opponent looks random -> defect *)
   Mod[deadlock, 4] == 0 && deadlock > 0, 1,  (* break deadlock -> cooperate *)
   True, Last[opp]]
];

(* Hard-majority: defect unless opponent has cooperated strictly more. *)
hardMajority[me_, opp_] := If[opp === {}, 0,
  If[Count[opp, 1] > Count[opp, 0], 1, 0]];

extendedStrategies = <|
  "ContriteTFT" -> contriteTFT,
  "Remorse"     -> remorse,
  "Adaptive"    -> adaptive,
  "OmegaTFT"    -> omegaTFT,
  "HardMajority"-> hardMajority
|>;

End[];
EndPackage[];
