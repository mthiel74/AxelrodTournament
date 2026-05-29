(* champion: TF2T + contrition  (sweep score 2.3626) *)
candidate[me_,opp_]:=Which[Length[opp]<2,1,opp[[-1]]==0&&opp[[-2]]==0&&!(me=!={}&&Last[me]==0),0,True,1];