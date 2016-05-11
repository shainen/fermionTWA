(* ::Package:: *)

tmax=30;
steps=1000;
times=Range[0,tmax,tmax/(steps-1)];


runs=100;


length=8;


sites=length;


numbos = sites;


numferm = 2 sites;


midPairs=Flatten[{Table[Table[{ii,jj},{jj,ii,sites}],{ii,sites}],Table[Table[{ii,jj},{jj,ii,2length}],{ii,length+1,2length}]},2];
lowPairs=Flatten[Table[{ii,jj},{ii,sites},{jj,sites+1,2sites}],1];


bonds=Table[{n,Mod[n+1,length,1]},{n,length}];


(*bonds={{1,2}};*)


(*bonds=Flatten[{{#,nfc[cfneither[#]+{0,0,1}]},{#,nfc[cfneither[#]+{0,1,0}]}}&/@Range[numferm],1];*)


coh=Table[0,{numbos}];


(*occupied={1,3};*)


occupied=Join[Range[2,sites,4],Range[4,sites,4]+sites];


\[Omega][t_]:=10


(*dis={1,-1};*)


\[CapitalDelta]=3;
\[Beta]=E;
\[Phi]=1;


(*dis=RandomReal[{-1,1},sites];*)


dis=N@\[CapitalDelta] Cos[2\[Pi] \[Beta] # +\[Phi]]&/@Range[sites];


g[t_]:=1
