(* champion: GTFT, forgive 30%  (sweep score 2.3499) *)
candidate[me_,opp_]:=If[opp==={},1,If[Last[opp]==1,1,If[RandomReal[]<0.3,1,0]]];