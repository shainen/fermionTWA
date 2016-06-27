(* ::Package:: *)

(* ::Section:: *)
(*All*)


(* ::Subsubsection:: *)
(*setup*)


(*SetDirectory[NotebookDirectory[]]*)


SetDirectory[Directory[]<>"/fermionTWA"];


<<randomSeed.wl


<<dynComplex.wl


(*<<dynBoson.wl*)


<<constSlaveHubbardMBL.wl


<<eqsSlaveHubbardMBL.wl


<<initsComplex.wl


<<initsDisc.wl


<<ndsolve.wl


(* ::Subsection:: *)
(*run TWA*)


Dynamic[rr]


observables={Em[#,#]&/@Range[numferm],bh[#]&/@Range[numbos]
(*,Flatten[{Table[Table[Em[ii,jj],{jj,ii+1,numferm}],{ii,numferm-1}],Table[Table[El[ii,jj],{jj,ii+1,numferm}],{ii,numferm-1}]}]*)
};
obsfun=Function[{values},
{values[[1]]+1/2,Abs[values[[2]]]^2-1/2,
(*Total[(values[[1]]\[Transpose])^2]/2+Total[Abs[values[[3]]\[Transpose]]^2]*)
values[[2]]
}
];


start=makeDSolveStart[observables];


fullTWA=0;
t2=Table[
Timing[
fullTWA+=runRandomInits[start,obsfun]/runs;
][[1]]
,{rr,runs}];


(*eachTWAC={};
Table[
AppendTo[eachTWAC,runRandomInitsC[startC,obsfun2]/runs];
,{rr,runs}];
fullTWAC=Total[eachTWAC];*)


nfup=fullTWA[[1]]\[Transpose][[1;;sites]];
nfdown=fullTWA[[1]]\[Transpose][[sites+1;;2sites]];
nbvac=fullTWA[[2]]\[Transpose][[1;;sites]];
nbfull=fullTWA[[2]]\[Transpose][[sites+1;;2sites]];


ncup=nfup+nbfull;
ncdown=nfdown+nbfull;


nEven=Total[(nfup+nfdown)[[Range[2,sites,2]]]];
nOdd=Total[(nfup+nfdown)[[Range[1,sites,2]]]];
imb=(nEven-nOdd)/(nEven+nOdd);


mmu=MaxMemoryUsed[]/10.^6;


SetDirectory[ParentDirectory[]];


Save["dataFermion.dat",{mmu,nfup,nfdown,nbvac,nbfull,ncup,ncdown,imb}];
