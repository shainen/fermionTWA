(* ::Package:: *)

(* ::Section:: *)
(*All*)


(* ::Subsubsection:: *)
(*setup*)


(*SetDirectory[NotebookDirectory[]]*)


SetDirectory[Directory[]<>"/fermionTWA"];


<<randomSeed.wl


<<2dfuncs.wl


<<dynComplex.wl


<<dynSpin.wl


<<constSlaveHubbardMBL.wl


<<eqsSHHCMBL.wl


<<initsComplexMom.wl


<<initsDisc.wl


<<ndsolve.wl


(* ::Subsection:: *)
(*run TWA*)


Dynamic[rr]


observables={Em[#,#]&/@Range[numferm]
,Sm[#]&/@Range[numbos]
,Em[#1,#2]&@@@midPairs
,El[#1,#2]&@@@lowPairs
,Sz[#]&/@Range[numbos]
};
obsfun=Function[{values},
{values[[1]]+1/2,values[[5]]+1/2
,values[[2]],values[[2]]\[Conjugate]values[[2]]+values[[5]]\[Conjugate]values[[5]]
(*,(values[[3,All,2]]+values[[3,All,5]])\[Conjugate] (values[[2,All,1]]\[Conjugate]values[[2,All,2]]-values[[2,All,3]]\[Conjugate]values[[2,All,4]])+(-values[[4,All,2]]-values[[4,All,3]])\[Conjugate](values[[2,All,2]]values[[2,All,3]]+values[[2,All,1]]values[[2,All,4]])
*)}
];


start=makeDSolveStart[observables];


eachTWA={};
Table[
AppendTo[eachTWA,runDiscSHHC[start,obsfun]/runs];
,{rr,runs}];
fullTWA=Total[eachTWA];


nfup=fullTWA[[1]]\[Transpose][[1;;sites]];
nfdown=fullTWA[[1]]\[Transpose][[sites+1;;2sites]];
nbvac=fullTWA[[2]]\[Transpose][[1;;sites]];
nbfull=fullTWA[[2]]\[Transpose][[sites+1;;2sites]];


ncup=nfup+nbfull;
ncdown=nfdown+nbfull;


nEven=Total[(nfup+nfdown)[[Range[2,sites,2]]]];
nOdd=Total[(nfup+nfdown)[[Range[1,sites,2]]]];
imb=(nEven-nOdd)/(nEven+nOdd);


(*kinTermS=fullTWA[[3]]+fullTWA[[3]]\[Conjugate];*)


mmu=MaxMemoryUsed[]/10.^6;


SetDirectory[ParentDirectory[]];


Save["dataFermion.dat",{mmu,nfup,nfdown,nbvac,nbfull,ncup,ncdown,nEven,nOdd,imb}];
