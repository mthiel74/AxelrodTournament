#!/usr/bin/env wolframscript
Get[FileNameJoin[{ParentDirectory@DirectoryName[$InputFileName],"wolfram","axelrod.wl"}]];
SeedRandom[42];
s = Axelrod`strategies;
pm[a_,b_]:=Axelrod`playMatch[s[a],s[b],200,0];
Print["names: ", Axelrod`strategyNames];
Print["TFT  vs TFT : ", pm["TitForTat","TitForTat"], "  expect {600,600}"];
Print["AllC vs AllC: ", pm["AllC","AllC"], "  expect {600,600}"];
Print["AllD vs AllD: ", pm["AllD","AllD"], "  expect {200,200}"];
Print["AllC vs AllD: ", pm["AllC","AllD"], "  expect {0,1000}"];
Print["TFT  vs AllD: ", pm["TitForTat","AllD"], "  expect {199,204}"];
Print["Grim vs AllD: ", pm["Grim","AllD"], "  expect {199,204}"];
Print["Pavlov vs AllD: ", pm["Pavlov","AllD"], "  (WSLS alternates -> exploited some)"];
Print["TF2T vs AllD: ", pm["TitFor2Tats","AllD"], "  expect TF2T tolerant"];
Print["--- ZD extortioner: enforces sX-P = 2(sY-P) vs ANY opponent ---"];
P=1; R=3;
Do[{sx,sy}=Axelrod`meanMatch[s["ZD-Extort-2"],s[o],2000,0,4];
   Print["  vs ",o,":  (sX-P)-2(sY-P) = ", NumberForm[(sx-P)-2(sy-P),{6,4}]],
   {o,{"AllC","AllD","TitForTat","Random","Pavlov","Grim"}}];
Print["--- ZD generous: enforces sX-R = 2(sY-R) ---"];
Do[{sx,sy}=Axelrod`meanMatch[s["ZD-Generous"],s[o],2000,0,4];
   Print["  vs ",o,":  (sX-R)-2(sY-R) = ", NumberForm[(sx-R)-2(sy-R),{6,4}]],
   {o,{"AllC","AllD","TitForTat","Random","Pavlov"}}];
Print["DONE"];
