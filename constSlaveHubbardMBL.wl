(* ::Package:: *)

(* ::Subsection:: *)
(*Sys*)


tmax=35;
steps=1000;
times=Range[0,tmax,tmax/(steps-1)];


(*tminExp=-1;
tmaxExp=3;
tmax=10.^tmaxExp;
steps=1000;
tExps=Range[tminExp,tmaxExp,(tmaxExp-tminExp)/(steps-1)];
times=10.^#&/@tExps;*)


runs=10;


length=40;


sites=length;


numbos = 2 sites;


numferm = 2 sites;


midPairs=Flatten[{Table[Table[{ii,jj},{jj,ii,sites}],{ii,sites}],Table[Table[{ii,jj},{jj,ii,2length}],{ii,length+1,2length}]},2];
lowPairs=Flatten[Table[{ii,jj},{ii,sites},{jj,sites+1,2sites}],1];


bonds=Table[{n,Mod[n+1,length,1]},{n,length-1}];


(* ::Subsection:: *)
(*Initial conditions*)


numDoub=6;


numOd=0;


evens=Range[2,sites,2];


odds=Range[1,sites,2];


doubles=RandomSample[evens,numDoub];


ups=RandomSample[Complement[evens,doubles],(length/2-numDoub)/2];


downs=Complement[Complement[evens,doubles],ups];


extraOdds=RandomSample[odds,numOd];


occupied=Join[ups,downs+sites,extraOdds];


empty=Complement[odds,extraOdds];


boseOc=Join[empty,doubles+sites];


coh=Normal[SparseArray[#->1&/@boseOc,{numbos}]];


occupiedB=boseOc;


(* ::Subsection:: *)
(*Ham consts*)


\[CapitalDelta]=3.0;
\[Beta]=0.721;
\[Phi]=RandomReal[{0,2\[Pi]}];


(*dis=RandomReal[{-1,1},sites];*)


dis=N@\[CapitalDelta] Cos[2\[Pi] \[Beta] # +\[Phi]]&/@Range[sites];


hopt[t_] := 1


intU[t_] := 4.7
