(* champion: GTFT, forgive 10% of defections  (sweep score 2.3299) *)
candidate[me_,opp_]:=If[opp==={},1,If[Last[opp]==1,1,If[RandomReal[]<0.1,1,0]]];