(* ::Package:: *)

tmax=35;
steps=1000;
times=Range[0,tmax,tmax/(steps-1)];


runs=10;


length=40;


sites=length;


numbos = sites;


numferm = 2 sites;


midPairs=Flatten[{Table[Table[{ii,jj},{jj,ii,sites}],{ii,sites}],Table[Table[{ii,jj},{jj,ii,2length}],{ii,length+1,2length}]},2];
lowPairs=Flatten[Table[{ii,jj},{ii,sites},{jj,sites+1,2sites}],1];


bonds=Table[{n,Mod[n+1,length,1]},{n,length-1}];


(*bonds={{1,2}};*)


(*bonds=Flatten[{{#,nfc[cfneither[#]+{0,0,1}]},{#,nfc[cfneither[#]+{0,1,0}]}}&/@Range[numferm],1];*)


coh=Table[0,{numbos}];


(*occupied={1,3};*)


(*occupied=Join[Range[2,sites,4],Range[4,sites,4]+sites];*)


evens=Range[2,sites,2];


doubles=RandomSample[evens,6];


ups=RandomSample[Complement[evens,doubles],7];


downs=Complement[Complement[evens,doubles],ups];


occupied=Join[doubles,doubles+sites,ups,downs+sites];


(*ups=RandomSample[Range[2,sites,2],sites/4];*)


(*occupied=Join[ups,Complement[Range[2,sites,2],ups]+sites];*)


(*dis={1,-1};*)


\[CapitalDelta]=2.9;
\[Beta]=0.721;
\[Phi]=RandomReal[{0,2\[Pi]}];


(*dis=RandomReal[{-1,1},sites];*)


dis=N@\[CapitalDelta] Cos[2\[Pi] \[Beta] # +\[Phi]]&/@Range[sites];


g[t_]:=2.


\[Omega][t_]:=10.
