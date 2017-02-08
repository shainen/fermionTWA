(* ::Package:: *)

pathToStart="/projectnb/twambl/170207_3_startTWAde/rundefined/start.dat";


pathToJcoup="/projectnb/twambl/170207_1_s16f4/";


(*pathToStart="/Users/shainen/Dropbox/Research/fTWA/SYK model/data/start.dat";*)


(*pathToJcoup="/Users/shainen/Dropbox/Research/fTWA/SYK model/data/170207_1_s16f4/";*)


tmax=0.01;
steps=100;
times=N[Range[0,tmax-tmax/steps,tmax/steps]];
split=1;
splitTimes=Split[times,!Or@@Table[#1<m tmax/split<=#2,{m,split-1}]&];


runs=100;


length=16;


sites=length;


numferm=sites;


midPairs = Flatten[{Table[Table[{ii, jj}, {jj, ii, sites}], {ii, sites}]}, 2];
lowPairs = {};


(*bonds=Table[{n,Mod[n+1,length,1]},{n,length-1}];*)


(*bonds=Flatten[Table[Table[{i,j},{j,i+1,sites}],{i,sites-1}],1]*)


(*alpha=1;*)


(*hopt[t_] := 1.*)


(*intU[t_] := 0.*)


(*occupied=Join[nfc/@(Position[fermenergy,_?Negative,2]-1),nfc/@(Position[fermenergy,0.,2]-1),nfc/@(Position[fermenergy,_?Negative,2]-1)+sites,nfc/@(Position[fermenergy,0.,2]-1)+sites];*)


occupied={7,8,9,10};


(*occupied={1,2};*)


(*sym1 = 1;
sym2 = 0;
sym3 = 0;*)
