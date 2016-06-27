(* ::Package:: *)

tscale=10;


tmax=4 tscale;
steps=500;
times=Range[0,tmax,tmax/(steps-1)];


runs=1000;


length=2;


sites=length;


numferm=2 sites;


midPairs=Flatten[{Table[Table[{ii,jj},{jj,ii,length}],{ii,length}],Table[Table[{ii,jj},{jj,ii,2length}],{ii,length+1,2length}]},2];
lowPairs=Flatten[Table[{ii,jj},{ii,length},{jj,length+1,2length}],1];


(*bonds=Table[{n,Mod[n+1,length,1]},{n,length}]*)


bonds={{1,2}};


hopt[t_] := 1


intU[t_] := (1-E^(-t^2/tscale^2))


(*occupied=Join[nfc/@(Position[fermenergy,_?Negative,2]-1),nfc/@(Position[fermenergy,0.,2]-1),nfc/@(Position[fermenergy,_?Negative,2]-1)+sites,nfc/@(Position[fermenergy,0.,2]-1)+sites];*)


occupied={1,3};
