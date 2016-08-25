(* ::Package:: *)

tmax=275;
steps=1000;
times=Range[0,tmax,tmax/(steps-1)];


runs=10;


length=9;


sites=length;


numferm=2 sites;


(*midPairs=Flatten[{Table[Table[{ii,jj},{jj,ii,length}],{ii,length}],Table[Table[{ii,jj},{jj,ii,2length}],{ii,length+1,2length}]},2];
lowPairs={};*)


midPairs=Flatten[Table[Table[{ii,jj},{jj,ii,numferm}],{ii,numferm}],1];
lowPairs=Flatten[Table[Table[{ii,jj},{jj,ii+1,numferm}],{ii,numferm}],1];


(*bonds=Table[{n,Mod[n+1,length,1]},{n,length}]*)


(*occupied=Join[nfc/@(Position[fermenergy,_?Negative,2]-1),nfc/@(Position[fermenergy,0.,2]-1),nfc/@(Position[fermenergy,_?Negative,2]-1)+sites,nfc/@(Position[fermenergy,0.,2]-1)+sites];*)


occupied={1,2,3,4,5};


sym1=1;
sym2=0;
sym3=0;
