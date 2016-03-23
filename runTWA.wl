(* ::Package:: *)

(* ::Section:: *)
(*All*)


(* ::Subsubsection:: *)
(*setup*)


(*SetDirectory[NotebookDirectory[]]*)


SetDirectory[Directory[]<>"/fermionTWA"];


<<randomSeed.wl


<<dynComplex.wl


<<2dfuncs.wl


<<constTwoChannel.wl


<<eqsTwoChannel.wl


<<initsComplexMom.wl


<<ndsolve.wl


(* ::Subsection:: *)
(*run TWA*)


Dynamic[rr]


observables={Em[#,#]&/@Range[numferm],bh[#]&/@Range[numbos],
Flatten[{Table[Table[Em[ii,jj],{jj,ii+1,numferm}],{ii,numferm-1}],Table[Table[El[ii,jj],{jj,ii+1,numferm}],{ii,numferm-1}]}]};
obsfun=Function[{values},
{values[[1]]+1/2,Abs[values[[2]]]^2-1/2,Total[(values[[1]]\[Transpose])^2]/2+Total[Abs[values[[3]]\[Transpose]]^2]}
];


start=makeDSolveStart[observables];


fullTWA=0;
Table[
fullTWA+=singleRun[start,initsMom,obsfun]/runs;
,{rr,runs}];


ncup=fullTWA[[1]]\[Transpose][[1;;sites]];


ncdown=fullTWA[[1]]\[Transpose][[sites+1;;2sites]];


mmu=MaxMemoryUsed[]/10.^6;


SetDirectory[ParentDirectory[]];


Save["dataFermion.dat",{mmu,ncup,ncdown}];
