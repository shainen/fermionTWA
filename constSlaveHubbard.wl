(* ::Package:: *)

(*tscale=1;*)


tmax=20;
steps=1000;
times=Range[0,tmax,tmax/(steps-1)];


runs=1;


length=8;


sites=length;


(*coh = Join[Table[0,{sites}],Table[1,{sites}]]*)


(*coh = {0,1,1,0};*)


coh=Flatten[{Table[{0,1},{length/2}],Table[{1,0},{length/2}]}];


occupied={};


(*\[Omega][t_] := -20(1-2E^(-t^2/tscale^2))*)


hopt[t_] := 1


intU[t_] := 10


numbos = 2 sites;


numferm = 2 sites;


numfvars=2numferm^2-numferm;
