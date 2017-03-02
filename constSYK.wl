(* ::Package:: *)

pathToStart="/projectnb/twambl/170302_4_startTWAs9_both/rundefined/start.dat";


pathToStart="/projectnb/twambl/170302_5_startTWAs9_scv/rundefined/start.dat";


pathToStart="/projectnb/twambl/170216_3_startTWAs9/rundefined/start.dat";


(*pathToJcoup="/projectnb/twambl/170216_1_s9f6/";*)


(*pathToStart="/Users/shainen/Dropbox/Research/fTWA/SYK model/data/start.dat";*)


(*pathToJcoup="/Users/shainen/Dropbox/Research/fTWA/SYK model/data/170207_1_s16f4/";*)


tmax=0.01;
steps=100;
times=N[Range[0,tmax-tmax/steps,tmax/steps]];
split=1;
splitTimes=Split[times,!Or@@Table[#1<m tmax/split<=#2,{m,split-1}]&];


runs=1;


length=9;


sites=length;


numferm=sites;


(*midPairs = Flatten[{Table[Table[{ii, jj}, {jj, ii, sites}], {ii, sites}]}, 2];
lowPairs = {};*)


midPairs = Flatten[{Table[Table[{ii, jj}, {jj, ii, sites}], {ii, sites}]}, 2];
lowPairs = Flatten[{Table[Table[{ii, jj}, {jj, ii+1, sites}], {ii, sites}]}, 2];


(*bonds=Table[{n,Mod[n+1,length,1]},{n,length-1}];*)


(*bonds=Flatten[Table[Table[{i,j},{j,i+1,sites}],{i,sites-1}],1]*)


(*alpha=1;*)


(*hopt[t_] := 1.*)


(*intU[t_] := 0.*)


(*occupied=Join[nfc/@(Position[fermenergy,_?Negative,2]-1),nfc/@(Position[fermenergy,0.,2]-1),nfc/@(Position[fermenergy,_?Negative,2]-1)+sites,nfc/@(Position[fermenergy,0.,2]-1)+sites];*)


occupied=Range[6];


(*occupied={7,8,9,10};*)


sym1 = 1/2;
sym2 = 1/2;
sym3 = 0;
