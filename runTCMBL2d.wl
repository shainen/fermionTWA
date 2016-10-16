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


<<constTCMBL2d.wl


<<eqsTCMBL2d.wl


<<initsComplex.wl


<<ndsolve.wl


(* ::Subsection:: *)
(*run TWA*)


Dynamic[rr]


observables={(*Em[#1,#2]&@@@midPairs,El[#1,#2]&@@@lowPairs,bh[#]&/@Range[numbos]*)
Em[#,#]&/@Range[numferm],bh[#]&/@Range[numbos]
(*,Flatten[{Table[Table[Em[ii,jj],{jj,ii+1,numferm}],{ii,numferm-1}],Table[Table[El[ii,jj],{jj,ii+1,numferm}],{ii,numferm-1}]}]
*)};
obsfun=Function[{values},
{values[[1]]+1/2,Abs[values[[2]]]^2-1/2
(*,Total[(values[[1]]\[Transpose])^2]/2+Total[Abs[values[[3]]\[Transpose]]^2]*)
}
];


t1=Timing[start=makeDSolveStart[observables];];


fullTWA=0;
t2=Table[
Timing[
fullTWA+=runRandomInits[start,obsfun]/runs;
][[1]]
,{rr,runs}];


(*fullTWA=0;
t2=Timing[Table[
\[Phi]=2\[Pi] rr/runs;
dis=N@\[CapitalDelta] Cos[2\[Pi] \[Beta] # +\[Phi]]&/@Range[sites];
start=makeDSolveStart[observables];
fullTWA+=runMeanInits[start,obsfun]/runs;
,{rr,runs}];];*)


fermOc=fullTWA[[1]]\[Transpose];
boseOc=fullTWA[[2]]\[Transpose];


(*nEven=Total[fermOc[[Range[2,numferm,2]]]];
nOdd=Total[fermOc[[Range[1,numferm,2]]]];
imb=(nEven-nOdd)/(nEven+nOdd);*)


nEven=Total[fermOc[[evens]]+fermOc[[evens+sites]]];
nOdd=Total[fermOc[[odds]]+fermOc[[odds+sites]]];
imb=(nEven-nOdd)/(nEven+nOdd);


mmu=MaxMemoryUsed[]/10.^6;


SetDirectory[ParentDirectory[]];


allData=imb;


Save["dataFermion.dat",{mmu,t1,t2,\[Phi]1,\[Phi]2,occupied,times,fermOc,boseOc,allData}];
