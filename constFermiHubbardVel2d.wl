(* ::Package:: *)

tmax=10;
steps=1000;
times=N[Range[0,tmax-tmax/steps,tmax/steps]];


runs=100;


length=4;


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


(*bonds=Flatten[Table[Table[{i,j},{j,i+1,sites}],{i,sites-1}],1];*)


bondsHor=Flatten[Table[{nfc[{xx,yy}],nfc[{xx,yy}+{0,1}]},{xx,0,length-1},{yy,0,length-2}],1];


bondsPerp=Flatten[Table[{nfc[{xx,yy}],nfc[{xx,yy}+{1,0}]},{xx,0,length-2},{yy,0,length-1}],1];


bonds=Join[bondsHor,bondsPerp];


alpha=1;


hopt[t_] := 1.


intU[t_] := 0.


(*occupied=Join[nfc/@(Position[fermenergy,_?Negative,2]-1),nfc/@(Position[fermenergy,0.,2]-1),nfc/@(Position[fermenergy,_?Negative,2]-1)+sites,nfc/@(Position[fermenergy,0.,2]-1)+sites];*)


occupied=Join[#,#+sites]&[{6,7,10,11}];


sym1 = 1;
sym2 = 0;
sym3 = 0;
