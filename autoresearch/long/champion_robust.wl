(* discovered memory-two strategy (long_search) *)
candidate[me_, opp_] := Module[{n = Length[opp], m1, o1, m2, o2, p = {1., 1., 0.0028, 0.3294, 0.8305, 0.215, 1., 0.604, 1., 1., 0.024800000000000003, 0.0794, 0.8914000000000001, 0., 0.8777, 0.031400000000000004}, op = 0.8951},
  If[n == 0, Return[If[RandomReal[] < op, 1, 0]]];
  m1 = Last[me]; o1 = Last[opp];
  If[n == 1, m2 = 1; o2 = 1, m2 = me[[-2]]; o2 = opp[[-2]]];
  If[RandomReal[] < p[[1 + 8 (1-m1) + 4 (1-o1) + 2 (1-m2) + (1-o2)]], 1, 0]]
