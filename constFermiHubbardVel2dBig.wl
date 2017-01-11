(* ::Package:: *)

tmax=0.1;
steps=1000;
times=N[Range[0,tmax-tmax/steps,tmax/steps]];
split=1;
splitTimes=Split[times,!Or@@Table[#1<m tmax/split<=#2,{m,split-1}]&];


runs=1;


length=16;


sites=length^2;


numferm=2 sites;


(* ::Input:: *)
(*(*midPairs=Flatten[{Table[Table[{ii,jj},{jj,ii,length}],{ii,length}],Table[Table[{ii,jj},{jj,ii,2length}],{ii,length+1,2length}]},2];*)
(*lowPairs=Flatten[Table[{ii,jj},{ii,length},{jj,length+1,2length}],1];*)*)


(*midPairs=Flatten[Table[Table[{ii,jj},{jj,ii,numferm}],{ii,numferm}],1];
lowPairs={};*)


midPairs=Flatten[{Table[Table[{ii,jj},{jj,ii,sites}],{ii,sites}],Table[Table[{ii,jj},{jj,ii,2 sites}],{ii,sites+1,2 sites}]},2];
lowPairs={};


(*bonds=Table[{n,Mod[n+1,length,1]},{n,length}]*)


bonds=Flatten[Table[Table[{i,j},{j,i+1,sites}],{i,sites-1}],1];


alpha=1;


hopt[t_] := 1.


intU[t_] := 1.


(*occupied=Join[nfc/@(Position[fermenergy,_?Negative,2]-1),nfc/@(Position[fermenergy,0.,2]-1),nfc/@(Position[fermenergy,_?Negative,2]-1)+sites,nfc/@(Position[fermenergy,0.,2]-1)+sites];*)


middle=Flatten[Table[nfc[{i,j}],{i,4,11},{j,4,11}]];


occupied=Join[#,#+sites]&[middle];


sym1 = 1;
sym2 = 0;
sym3 = 0;
