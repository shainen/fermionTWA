(* ::Package:: *)

tmax=20;
steps=1000;
times=Range[0,tmax,tmax/(steps-1)];


(*tminExp=-1;
tmaxExp=3;
tmax=10.^tmaxExp;
steps=1000;
tExps=Range[tminExp,tmaxExp,(tmaxExp-tminExp)/(steps-1)];
times=10.^#&/@tExps;*)


runs=1;


length=12;


sites=length^2;


numbos = sites;


numferm = 2 sites;


midPairs=Flatten[{Table[Table[{ii,jj},{jj,ii,sites}],{ii,sites}],Table[Table[{ii,jj},{jj,ii,2sites}],{ii,sites+1,2sites}]},2];
lowPairs=Flatten[Table[{ii,jj},{ii,sites},{jj,sites+1,2sites}],1];


(*bonds=Table[{n,Mod[n+1,length,1]},{n,length}]*)


(*bonds={{1,2}};*)


bonds=Flatten[Table[{nfc[{xx,yy}],nfc[{xx,yy}+{0,1}]},{xx,0,length-1},{yy,0,length-2}],1];


bondsPerp=Flatten[Table[{nfc[{xx,yy}],nfc[{xx,yy}+{1,0}]},{xx,0,length-2},{yy,0,length-1}],1];


(*bonds={#,nfc[cfneither[#]+{0,0,1}]}&/@Range[sites]*)


(*bondsPerp={#,nfc[cfneither[#]+{0,1,0}]}&/@Range[sites]*)


coh=Table[0,{numbos}];


(*occupied={1,3};*)


(*occupied=Join[Range[2,sites,4],Range[4,sites,4]+sites];*)


numDoub=0;


numOd=0;


(*evens=Flatten[Table[Range[(3-(-1)^i)/2+(i-1)*length,i*length,2],{i,length}]];*)


evens=Flatten[Table[nfc[{xx,yy}],{xx,1,length-1,2},{yy,0,length-1}]];


(*odds=Range[1,sites,2];*)


odds=Complement[Range[sites],evens];


doubles=RandomSample[evens,numDoub];


ups=RandomSample[Complement[evens,doubles],(sites/2-numDoub)/2];


downs=Complement[Complement[evens,doubles],ups];


extraOdds=RandomSample[odds,numOd];


occupied=Join[doubles,doubles+sites,ups,downs+sites,extraOdds];


(*ups=RandomSample[Range[2,sites,2],sites/4];*)


(*occupied=Join[ups,Complement[Range[2,sites,2],ups]+sites];*)


(*dis={1,-1};*)


\[CapitalDelta]=8.0;
\[Beta]1=0.721;
\[Beta]2=0.692;
\[Phi]1=RandomReal[{0,2\[Pi]}];
\[Phi]2=RandomReal[{0,2\[Pi]}];


(*dis=RandomReal[{-1,1},sites];*)


dis=Flatten[Table[N@\[CapitalDelta] (Cos[2\[Pi] \[Beta]1 xx +\[Phi]1]+Cos[2\[Pi] \[Beta]2 yy +\[Phi]2])/2,{xx,length},{yy,length}]];


(*dis=N@\[CapitalDelta] RandomReal[{-1,1},{sites}];*)


jPerp=1.0;


g[t_]:=2.


\[Omega][t_]:=10.
