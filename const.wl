(* ::Package:: *)

(*tscale=20;*)


tmax=10;
steps=500;
times=Range[0,tmax,tmax/(steps-1)];


runs=1000;


length=2;


sites=length;


(*coh = Join[Table[0,{sites}],Table[1,{sites}]]*)


coh = {0,1,1,0};


occupied={};


(*\[Omega][t_] := -20(1-2E^(-t^2/tscale^2))*)


hopt[t_] := 1


intU[t_] := 5


numbos = 2 sites;


numferm = 2 sites;


numfvars=2numferm^2-numferm;
