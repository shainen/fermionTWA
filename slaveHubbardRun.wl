(* ::Package:: *)

(* ::Section:: *)
(*All*)


(* ::Subsubsection:: *)
(*setup*)


SetDirectory[NotebookDirectory[]]


(*SetDirectory[Directory[]<>"/fermionTWA"];*)


<<randomSeed.wl


<<dynComplex.wl


<<dynBoson.wl


<<constSlaveHubbard.wl


<<eqsSlaveHubbard.wl


<<eqsSlaveHubbardCompact.wl


<<initsComplex.wl


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


observables2={Em[#,#]&/@Range[numferm],Bm[#,#]&/@Range[numbos]
,Flatten[{Em[#1,#2]&@@@midPairs,El[#1,#2]&@@@lowPairs}]
,Flatten[{Bm[#1,#2]&@@@midPairs,Bl[#1,#2]&@@@lowPairs}]
};
obsfun2=Function[{values},
{values[[1]]+1/2,values[[2]]-1/2,values[[3]],values[[4]](*Total[(values[[1]]\[Transpose])^2]/2+Total[Abs[values[[3]]\[Transpose]]^2]*)}
];


start=makeDSolveStart[observables];


startC=makeDSolveStartCompact[observables2];


eachTWA={};
eachTWAC={};
Table[
startbs=randomBoseInits;
startEm=Em[#1,#2][0]==randomEm[#1,#2]&@@@midPairs;
startEl=El[#1,#2][0]==randomEl[#1,#2]&@@@lowPairs;
AppendTo[eachTWA,runRandomInits[start,obsfun]];
AppendTo[eachTWAC,runRandomInitsC[startC,obsfun2]];
,{rr,runs}];
fullTWA=Total[eachTWA]/runs;
fullTWAC=Total[eachTWAC]/runs;


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


nfupC=fullTWAC[[1]]\[Transpose][[1;;sites]];
nfdownC=fullTWAC[[1]]\[Transpose][[sites+1;;2sites]];
nbvacC=fullTWAC[[2]]\[Transpose][[1;;sites]];
nbfullC=fullTWAC[[2]]\[Transpose][[sites+1;;2sites]];


ncupC=nfupC+nbfullC;
ncdownC=nfdownC+nbfullC;


mmu=MaxMemoryUsed[]/10.^6;


SetDirectory[ParentDirectory[]];


Save["dataFermion.dat",{mmu,nfup,nfdown,nbvac,nbfull,ncup,ncdown}];
