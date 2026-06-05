(* ::Package:: *)

(* ===================================================================== *)
(* axelrod.wl  --  core engine for the iterated Prisoner's Dilemma.       *)
(*                                                                        *)
(* Loaded by the *.wls entry points via                                   *)
(*   Get[FileNameJoin[{DirectoryName[$InputFileName], "axelrod.wl"}]].    *)
(*                                                                        *)
(* Move encoding:  C = 1 (cooperate),  D = 0 (defect).                    *)
(* A strategy is a pure function  s[myHist, oppHist] -> 0|1  where        *)
(* myHist / oppHist are chronological lists of ACTUAL past moves          *)
(* (oldest first, most recent last); both empty on the opening move.      *)
(* Opponents observe ACTUAL (post-noise) moves -> execution-error model.  *)
(* ===================================================================== *)

BeginPackage["Axelrod`"];

ClearAll["Axelrod`*", "Axelrod`Private`*"];

payoff::usage           = "payoff[my,opp] gives the focal player's payoff for actual moves my,opp.";
$payoffs::usage         = "$payoffs = <|R,S,T,P|>, the four payoff constants.";
strategies::usage       = "strategies is an Association name -> strategy function.";
strategyNames::usage    = "strategyNames is the ordered list of strategy names.";
memOne::usage           = "memOne[open,{pCC,pCD,pDC,pDD}] builds a memory-one strategy.";
zdStrategy::usage       = "zdStrategy[chi,phi,anchor] builds a zero-determinant memory-one strategy.";
playMatch::usage        = "playMatch[sA,sB,rounds,noise] -> {totalA,totalB} for one match.";
meanMatch::usage        = "meanMatch[sA,sB,rounds,noise,reps] -> {meanPerRoundA,meanPerRoundB}.";
tournamentMatrix::usage = "tournamentMatrix[names,rounds,noise,reps] -> matrix M[[i,j]] = mean-per-round score of i vs j.";
fieldScores::usage      = "fieldScores[M] -> mean score of each strategy against the whole field (row means).";
ranking::usage          = "ranking[names,M] -> sorted list {rank,name,score} (descending), numeric-safe.";

Begin["`Private`"];

(* --- payoffs:  T=5 > R=3 > P=1 > S=0,  2R > T+S --------------------- *)
$R = 3; $S = 0; $T = 5; $P = 1;
$payoffs = <|"R" -> $R, "S" -> $S, "T" -> $T, "P" -> $P|>;
payoff[1, 1] = $R;   (* both cooperate              *)
payoff[1, 0] = $S;   (* I cooperate, opponent defects *)
payoff[0, 1] = $T;   (* I defect,   opponent cooperates *)
payoff[0, 0] = $P;   (* both defect                 *)

(* --- memory-one helper --------------------------------------------- *)
(* state index from (myLast,oppLast): (C,C)->1 (C,D)->2 (D,C)->3 (D,D)->4 *)
stateIndex[myLast_, oppLast_] := 1 + (1 - myLast)*2 + (1 - oppLast);

memOne[open_, p_List] := Function[{me, opp},
  If[me === {},
    If[RandomReal[] < open, 1, 0],
    If[RandomReal[] < p[[ stateIndex[Last[me], Last[opp]] ]], 1, 0]
  ]
];

(* --- zero-determinant generator (Press & Dyson 2012) --------------- *)
(* enforces  sX - anchor = chi (sY - anchor) for the focal player X.    *)
(*   anchor = $P -> extortionate ;  anchor = $R -> generous            *)
zdParameters[chi_, phi_, anchor_] := Module[{sx, sy, pt},
  sx = {$R, $S, $T, $P} - anchor;    (* X payoff in states CC,CD,DC,DD *)
  sy = {$R, $T, $S, $P} - anchor;    (* Y payoff in same states        *)
  pt = phi*(sx - chi*sy);
  {1 + pt[[1]], 1 + pt[[2]], pt[[3]], pt[[4]]}
];
zdStrategy[chi_, phi_, anchor_] := memOne[1, zdParameters[chi, phi, anchor]];

(* --- Gradual (Beaufils): on the opponent's k-th cumulative defection, *)
(*     retaliate with a burst of k defections (re-armed by new defects) *)
gradual[me_, opp_] := Module[{punish = 0, nDef = 0},
  Do[
    If[punish > 0, punish--];
    If[opp[[k]] == 0, nDef++; punish = nDef],
    {k, Length[opp]}
  ];
  If[punish > 0, 0, 1]
];

(* --- strategy library ---------------------------------------------- *)
strategies = <|
  "AllC"          -> memOne[1, {1, 1, 1, 1}],
  "AllD"          -> memOne[0, {0, 0, 0, 0}],
  "Random"        -> memOne[0.5, {0.5, 0.5, 0.5, 0.5}],
  "TitForTat"     -> memOne[1, {1, 0, 1, 0}],
  "SuspiciousTFT" -> memOne[0, {1, 0, 1, 0}],
  "GenerousTFT"   -> memOne[1, {1, 1/3, 1, 1/3}],
  "TitFor2Tats"   -> Function[{me, opp},
      If[Length[opp] >= 2 && opp[[-1]] == 0 && opp[[-2]] == 0, 0, 1]],
  "2TitsForTat"   -> Function[{me, opp},
      If[Length[opp] >= 1 && (opp[[-1]] == 0 ||
        (Length[opp] >= 2 && opp[[-2]] == 0)), 0, 1]],
  "Grim"          -> Function[{me, opp}, If[MemberQ[opp, 0], 0, 1]],
  "Pavlov"        -> memOne[1, {1, 0, 0, 1}],
  "Gradual"       -> gradual,
  "ZD-Extort-2"   -> zdStrategy[2, 1/9, $P],
  "ZD-Generous"   -> zdStrategy[2, 1/8, $R]
|>;

strategyNames = Keys[strategies];

(* --- match engine: intended moves flipped with prob = noise --------- *)
playMatch[sA_, sB_, rounds_, noise_] := Module[
  {histA = {}, histB = {}, mA, mB, aA, aB, sumA = 0, sumB = 0},
  Do[
    mA = sA[histA, histB];
    mB = sB[histB, histA];
    aA = If[noise > 0 && RandomReal[] < noise, 1 - mA, mA];
    aB = If[noise > 0 && RandomReal[] < noise, 1 - mB, mB];
    sumA += payoff[aA, aB];
    sumB += payoff[aB, aA];
    AppendTo[histA, aA]; AppendTo[histB, aB],
    {rounds}
  ];
  {sumA, sumB}
];

meanMatch[sA_, sB_, rounds_, noise_, reps_] :=
  N[Total[Table[playMatch[sA, sB, rounds, noise], {reps}]] / (rounds*reps)];

(* --- round-robin matrix (self-play included, as in Axelrod 1980) ---- *)
tournamentMatrix[names_List, rounds_, noise_, reps_] := Module[{n = Length[names], M},
  M = Table[0., {n}, {n}];
  Do[
    With[{ab = meanMatch[strategies[names[[i]]], strategies[names[[j]]], rounds, noise, reps]},
      M[[i, j]] = ab[[1]]; M[[j, i]] = ab[[2]]],
    {i, n}, {j, i, n}
  ];
  M
];

fieldScores[M_] := N[Mean /@ M];

ranking[names_List, M_] := Module[{sc = fieldScores[M], order},
  order = Reverse[Ordering[N[sc]]];   (* numeric-safe descending order *)
  Table[{r, names[[order[[r]]]], sc[[order[[r]]]]}, {r, Length[names]}]
];

End[];
EndPackage[];
