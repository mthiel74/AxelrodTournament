#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  tournament.wls -- Phase 2: round-robin tournament (Axelrod 1980).
    Builds the mean-per-round payoff matrix at several noise levels,
    ranks strategies by mean score against the field, and writes
    rankings (CSV) + the full matrices (CSV + MX) to data/.            *)

Get[FileNameJoin[{DirectoryName[$InputFileName], "axelrod.wl"}]];
repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
dataDir  = FileNameJoin[{repoRoot, "data"}];
If[! DirectoryQ[dataDir], CreateDirectory[dataDir]];

SeedRandom[2026];
rounds = 200;
reps   = 100;
noises = {0., 0.01, 0.05, 0.1};
names  = Axelrod`strategyNames;

Print["[tournament] ", Length[names], " strategies, ", rounds,
  " rounds, ", reps, " reps, noise ", noises];

allM = <||>;
Do[
  Module[{M, rk, t0 = AbsoluteTime[]},
    M  = Axelrod`tournamentMatrix[names, rounds, eps, reps];
    rk = Axelrod`ranking[names, M];
    allM[eps] = M;
    Print["[tournament] noise=", eps, "  (",
      Round[AbsoluteTime[] - t0, 0.1], "s)"];
    Do[Print["   ",
      StringPadLeft[ToString[r[[1]]], 3], ". ",
      StringPadRight[r[[2]], 16], NumberForm[r[[3]], {5, 3}]], {r, rk}];
    Export[FileNameJoin[{dataDir,
        "ranking_noise" <> ToString[eps] <> ".csv"}],
      Prepend[rk, {"rank", "strategy", "meanScore"}]];
    (* matrix CSV with header row + column of strategy names *)
    Export[FileNameJoin[{dataDir,
        "matrix_noise" <> ToString[eps] <> ".csv"}],
      Join[{Prepend[names, "vs"]}, MapThread[Prepend, {M, names}]]];
  ],
  {eps, noises}
];

Export[FileNameJoin[{dataDir, "tournament.mx"}],
  <|"names" -> names, "noises" -> noises, "rounds" -> rounds,
    "reps" -> reps, "matrices" -> allM|>];

Print["[tournament] wrote rankings + matrices to ", dataDir];
