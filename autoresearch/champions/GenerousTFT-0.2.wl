(* champion: GTFT, forgive 20%  (sweep score 2.3314) *)
candidate[me_,opp_]:=If[opp==={},1,If[Last[opp]==1,1,If[RandomReal[]<0.2,1,0]]];