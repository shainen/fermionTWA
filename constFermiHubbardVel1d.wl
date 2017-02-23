(* ::Package:: *)

tmax=10;
steps=1000;
times=N[Range[0,tmax-tmax/steps,tmax/steps]];


runs=1000;


length=16;


sites=length;


numferm=2 sites;


(* ::Input:: *)
(*(*midPairs=Flatten[{Table[Table[{ii,jj},{jj,ii,length}],{ii,length}],Table[Table[{ii,jj},{jj,ii,2length}],{ii,length+1,2length}]},2];*)
(*lowPairs=Flatten[Table[{ii,jj},{ii,length},{jj,length+1,2length}],1];*)*)


(*midPairs=Flatten[Table[Table[{ii,jj},{jj,ii,numferm}],{ii,numferm}],1];
lowPairs={};*)


midPairs=Flatten[{Table[Table[{ii,jj},{jj,ii,length}],{ii,length}],Table[Table[{ii,jj},{jj,ii,2length}],{ii,length+1,2length}]},2];
lowPairs={};


bonds=Table[{n,Mod[n+1,length,1]},{n,length-1}];


(*bonds=Flatten[Table[Table[{i,j},{j,i+1,sites}],{i,sites-1}],1]*)


alpha=1;


hopt[t_] := 1.


intU[t_] := 0.


(*occupied=Join[nfc/@(Position[fermenergy,_?Negative,2]-1),nfc/@(Position[fermenergy,0.,2]-1),nfc/@(Position[fermenergy,_?Negative,2]-1)+sites,nfc/@(Position[fermenergy,0.,2]-1)+sites];*)


occupied=Join[#,#+sites]&[{7,8,9,10}];


sym1 = 1;
sym2 = 0;
sym3 = 0;
