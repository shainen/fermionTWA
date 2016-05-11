(* ::Package:: *)

tscale=10;


tmax=tscale;
steps=1000;
times=Range[0,tmax,tmax/(steps-1)];


runs=1;


length=10;


sites=length^2;


numbos = sites;


numferm = 2 sites;


numfvars=2numferm^2-numferm;


coh=Table[0,{numbos}];


fermenergy=Table[N[-2(Cos[2\[Pi] ii/length]+Cos[2\[Pi] jj/length])],{ii,0,length-1},{jj,0,length-1}];


occupied=Join[nfc/@(Position[fermenergy,_?Negative,2]-1),nfc/@(Position[fermenergy,0.,2]-1),nfc/@(Position[fermenergy,_?Negative,2]-1)+sites,nfc/@(Position[fermenergy,0.,2]-1)+sites];


bonds=Flatten[{{#,nfc[cfneither[#]+{0,0,1}]},{#,nfc[cfneither[#]+{0,1,0}]}}&/@Range[numferm],1];


finMu=10;


\[Omega][t_]:=-finMu(1-E^(-t^2/tscale^2))


(*\[Omega][t_]:=0*)


g[t_]:=(1-E^(-t^2/tscale^2))


(*g[t_]:=0*)
