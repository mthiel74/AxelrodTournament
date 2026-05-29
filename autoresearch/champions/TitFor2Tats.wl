(* champion: retaliate only after 2 defections  (sweep score 2.4004) *)
candidate[me_,opp_]:=If[Length[opp]>=2&&opp[[-1]]==0&&opp[[-2]]==0,0,1];