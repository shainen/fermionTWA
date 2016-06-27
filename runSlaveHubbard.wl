(* ::Package:: *)

(* ::Section:: *)
(*All*)


(* ::Subsubsection:: *)
(*setup*)


SetDirectory[NotebookDirectory[]]


(*SetDirectory[Directory[]<>"/fermionTWA"];*)


<<randomSeed.wl


<<2dfuncs.wl


<<dynComplex.wl


<<dynBoson.wl


<<constSlaveHubbard.wl


<<eqsSlaveHubbard.wl


<<eqsSlaveHubbardCompact.wl


<<initsComplexMom.wl


<<initsDisc.wl


<<ndsolve.wl


(* ::Subsection:: *)
(*run TWA*)


Dynamic[rr]


(*observables={Em[#,#]&/@Range[numferm],bh[#]&/@Range[numbos]
,Em[#1,#2]&@@@midPairs,El[#1,#2]&@@@lowPairs
};
obsfun=Function[{values},
{values[[1]]+1/2,Abs[values[[2]]]^2-1/2,
(*Total[(values[[1]]\[Transpose])^2]/2+Total[Abs[values[[3]]\[Transpose]]^2]*)
(*(values[[3,2]]+values[[3,5]])\[Conjugate] (values[[2,1]]\[Conjugate]values[[2,2]]-values[[2,3]]\[Conjugate]values[[2,4]])+(-values[[4,2]]-values[[4,3]])\[Conjugate](values[[2,2]]values[[2,3]]+values[[2,1]]values[[2,4]])*)
(values[[3,All,2]]+values[[3,All,5]])\[Conjugate] (values[[2,All,1]]\[Conjugate]values[[2,All,2]]-values[[2,All,3]]\[Conjugate]values[[2,All,4]])+(-values[[4,All,2]]-values[[4,All,3]])\[Conjugate](values[[2,All,2]]values[[2,All,3]]+values[[2,All,1]]values[[2,All,4]])

}
];*)


observables2={Em[#,#]&/@Range[numferm],Bm[#,#]&/@Range[numbos]
,Flatten[{Em[#1,#2]&@@@midPairs,El[#1,#2]&@@@lowPairs}]
,Flatten[{Bm[#1,#2]&@@@midPairs,Bl[#1,#2]&@@@lowPairs}]
,{Total[( 
(hEm[#1,#2]+hEm[#1+sites,#2+sites])
(bh[#2][t]\[Conjugate]bh[#1][t]-bh[#2+sites][t]\[Conjugate]bh[#1+sites][t])
+(hEu[#1,#2+sites]-hEu[#1+sites,#2])
(bh[#1][t]bh[#2+sites][t]+bh[#1+sites][t]bh[#2][t])
+(hEm[#2,#1]+hEm[#2+sites,#1+sites])
(bh[#2][t]bh[#1][t]\[Conjugate]-bh[#2+sites][t]bh[#1+sites][t]\[Conjugate])
+(hEl[#2+sites,#1]-hEl[#2,#1+sites])
(bh[#1][t]\[Conjugate]bh[#2+sites][t]\[Conjugate]+bh[#1+sites][t]\[Conjugate]bh[#2][t]\[Conjugate])
)&@@@bonds][[1]]}
};
obsfun2=Function[{values},
{values[[1]]+1/2,values[[2]]-1/2,values[[3]](*,values[[4]]Total[(values[[1]]\[Transpose])^2]/2+Total[Abs[values[[3]]\[Transpose]]^2]*)}
];


(*start=makeDSolveStart[observables];*)


startC=makeDSolveStartCompact[observables2];


(*eachTWA={};
eachTWAC={};
Table[
startbs=randomBoseInits;
startEm=Em[#1,#2][0]==randomEm[#1,#2]&@@@midPairs;
startEl=El[#1,#2][0]==randomEl[#1,#2]&@@@lowPairs;
AppendTo[eachTWA,runRandomInits[start,obsfun]];
AppendTo[eachTWAC,runRandomInitsC[startC,obsfun2]];
,{rr,runs}];
fullTWA=Total[eachTWA]/runs;
fullTWAC=Total[eachTWAC]/runs;*)


(*eachTWA={};
Table[
AppendTo[eachTWA,runMomInits[start,obsfun]/runs];
,{rr,runs}];
fullTWA=Total[eachTWA];*)


eachTWAC={};
Table[
AppendTo[eachTWAC,runRandomInitsCDisc[startC,obsfun2]/runs];
,{rr,runs}];
fullTWAC=Total[eachTWAC];


(*nfup=fullTWA[[1]]\[Transpose][[1;;sites]];
nfdown=fullTWA[[1]]\[Transpose][[sites+1;;2sites]];
nbvac=fullTWA[[2]]\[Transpose][[1;;sites]];
nbfull=fullTWA[[2]]\[Transpose][[sites+1;;2sites]];*)


(*ncup=nfup+nbfull;
ncdown=nfdown+nbfull;*)


(*kinTermS=fullTWA[[3]]+fullTWA[[3]]\[Conjugate];*)


nfupC=fullTWAC[[1]]\[Transpose][[1;;sites]];
nfdownC=fullTWAC[[1]]\[Transpose][[sites+1;;2sites]];
nbvacC=fullTWAC[[2]]\[Transpose][[1;;sites]];
nbfullC=fullTWAC[[2]]\[Transpose][[sites+1;;2sites]];


ncupC=nfupC+nbfullC;
ncdownC=nfdownC+nbfullC;


mmu=MaxMemoryUsed[]/10.^6;


SetDirectory[ParentDirectory[]];


(*Save["dataFermion.dat",{mmu,nfup,nfdown,nbvac,nbfull,ncup,ncdown}];*)
