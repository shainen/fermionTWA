(* ::Package:: *)

tmax=10;
steps=1000;
times=N[Range[0,tmax-tmax/steps,tmax/steps]];


runs=100;


length=15;


sites=length;


numferm=2 sites;


midPairs=Flatten[{Table[Table[{ii,jj},{jj,ii,length}],{ii,length}],Table[Table[{ii,jj},{jj,ii,2length}],{ii,length+1,2length}]},2];
lowPairs=Flatten[Table[{ii,jj},{ii,length},{jj,length+1,2length}],1];


(*bonds=Table[{n,Mod[n+1,length,1]},{n,length}]*)


bonds=Table[{i,Mod[i+1,sites,1]},{i,sites-1}];


hopt[t_] := 1


intU[t_] := 1


(*occupied=Join[nfc/@(Position[fermenergy,_?Negative,2]-1),nfc/@(Position[fermenergy,0.,2]-1),nfc/@(Position[fermenergy,_?Negative,2]-1)+sites,nfc/@(Position[fermenergy,0.,2]-1)+sites];*)


occupied=Join[#,#+sites]&[{7,8,9}];
