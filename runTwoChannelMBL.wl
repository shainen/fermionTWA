(* ::Package:: *)

(* ::Section:: *)
(*All*)


(* ::Subsubsection:: *)
(*setup*)


SetDirectory[NotebookDirectory[]]


(*SetDirectory[Directory[]<>"/fermionTWA"];*)


<<randomSeed.wl


<<dynComplex.wl


(*<<2dfuncs.wl*)


<<constTwoChannelMBL.wl


<<eqsTwoChannelMBL.wl


<<initsComplex.wl


<<ndsolve.wl


(* ::Subsection:: *)
(*run TWA*)


Dynamic[rr]


observables={Em[#,#]&/@Range[numferm],bh[#]&/@Range[numbos]
(*,Flatten[{Table[Table[Em[ii,jj],{jj,ii+1,numferm}],{ii,numferm-1}],Table[Table[El[ii,jj],{jj,ii+1,numferm}],{ii,numferm-1}]}]
*)};
obsfun=Function[{values},
{values[[1]]+1/2,Abs[values[[2]]]^2-1/2
(*,Total[(values[[1]]\[Transpose])^2]/2+Total[Abs[values[[3]]\[Transpose]]^2]*)
}
];


t1=Timing[start=makeDSolveStart[observables];];


fullTWA=0;
t2=Timing[Table[
fullTWA+=runRandomInits[start,obsfun]/runs;
,{rr,runs}];];


fermOc=fullTWA[[1]]\[Transpose];
boseOc=fullTWA[[2]]\[Transpose];


nEven=Total[fermOc[[Range[2,numferm,2]]]];
nOdd=Total[fermOc[[Range[1,numferm,2]]]];
imb=(nEven-nOdd)/(nEven+nOdd);


mmu=MaxMemoryUsed[]/10.^6;


SetDirectory[ParentDirectory[]];


(*Save["dataFermion.dat",{mmu,t1,t2,t3,nbos,fermMomNums}];*)
