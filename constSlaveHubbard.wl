(* ::Package:: *)

(*tscale=1;*)


tmax=5;
steps=1000;
times=Range[0,tmax,tmax/(steps-1)];


runs=100;


length=2;


sites=length;


(*coh = Join[Table[0,{sites}],Table[1,{sites}]]*)


(*coh = {1/Sqrt[2],1/Sqrt[2],1/Sqrt[2],1/Sqrt[2]};*)


(*coh={1,0,1,0,0,0,0,0}*)


(*coh=Flatten[{Table[{0,1},{length/2}],Table[{1,0},{length/2}]}];*)


hopt[t_] := 1.


(*intU[t_] := 5(1-E^(-t^2/tscale^2))*)


intU[t_] := 10


numbos = 2 sites;


numferm = 2 sites;


midPairs=Flatten[{Table[Table[{ii,jj},{jj,ii,length}],{ii,length}],Table[Table[{ii,jj},{jj,ii,2length}],{ii,length+1,2length}]},2];
lowPairs=Flatten[Table[{ii,jj},{ii,length},{jj,length+1,2length}],1];


bonds=Table[{n,Mod[n+1,length,1]},{n,length}];


(*bonds={{1,2}};*)


occupied={};


occupiedB={1,4};
