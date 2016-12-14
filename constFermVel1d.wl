(* ::Package:: *)

(*tscale=10;*)


tmax=10;
steps=1000;
times=N[Range[0,tmax-tmax/steps,tmax/steps]];


runs=100;


length=15;


sites=length;


numbos = sites;


numferm = 2 sites;


midPairs=Flatten[{Table[Table[{ii,jj},{jj,ii,sites}],{ii,sites}],Table[Table[{ii,jj},{jj,ii,2length}],{ii,length+1,2length}]},2];
lowPairs=Flatten[Table[{ii,jj},{ii,sites},{jj,sites+1,2sites}],1];


(*numfvars=2numferm^2-numferm;*)


coh=Table[0,{numbos}];


(*fermenergy=Table[N[-2(Cos[2\[Pi] ii/length]+Cos[2\[Pi] jj/length])],{ii,0,length-1},{jj,0,length-1}];*)


(*occupied=Join[nfc/@(Position[fermenergy,_?Negative,2]-1),nfc/@(Position[fermenergy,0.,2]-1),nfc/@(Position[fermenergy,_?Negative,2]-1)+sites,nfc/@(Position[fermenergy,0.,2]-1)+sites];*)


(*bonds=Flatten[{{#,nfc[cfneither[#]+{0,0,1}]},{#,nfc[cfneither[#]+{0,1,0}]}}&/@Range[numferm],1];*)


occupied=Join[#,#+sites]&[{7,8,9}];


bonds=Table[{i,Mod[i+1,sites,1]},{i,sites}];


(*finMu=10;*)


(*\[Omega][t_]:=-finMu(1-E^(-t^2/tscale^2))*)


\[Omega][t_]:=5.


(*g[t_]:=(1-E^(-t^2/tscale^2))*)


g[t_]:=1.
