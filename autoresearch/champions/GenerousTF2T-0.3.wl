(* champion: TF2T with 30% forgiveness leak  (sweep score 2.3947) *)
candidate[me_,opp_]:=Which[Length[opp]<2,1,opp[[-1]]==0&&opp[[-2]]==0,If[RandomReal[]<0.3,1,0],True,1];