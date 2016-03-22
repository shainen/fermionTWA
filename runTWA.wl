(* ::Package:: *)

(* ::Section:: *)
(*All*)


(* ::Subsubsection:: *)
(*setup*)


(*SetDirectory[NotebookDirectory[]]*)


<<randomSeed.wl


SetDirectory[Directory[]<>"/fermionTWA"];


<<dynComplex.wl


<<const.wl


<<makeEqs.wl


<<initsComplex.wl


<<ndsolve.wl


(* ::Subsection:: *)
(*run TWA*)


Dynamic[rr]


observables={Em[#,#]&/@Range[numferm],bh[#]&/@Range[numbos],
Flatten[{Table[Table[Em[ii,jj],{jj,ii+1,numferm}],{ii,numferm-1}],Table[Table[El[ii,jj],{jj,ii+1,numferm}],{ii,numferm-1}]}]};
start=makeDSolveStart[observables];


fullTWA=0;
Table[
fullTWA+=runRandomInits[start]/runs;
,{rr,runs}];


nfup=fullTWA[[1]]\[Transpose][[1;;sites]];


nfdown=fullTWA[[1]]\[Transpose][[sites+1;;2sites]];


nbvac=fullTWA[[2]]\[Transpose][[1;;sites]];


nbfull=fullTWA[[2]]\[Transpose][[sites+1;;2sites]];


ncup=nfup+nbfull;
ncdown=nfdown+nbfull;


mmu=MaxMemoryUsed[]/10.^6;


SetDirectory[ParentDirectory[]];


Save["dataFermion.dat",{mmu,nfup,nfdown,nbvac,nbfull,ncup,ncdown}];
