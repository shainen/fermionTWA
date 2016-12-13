(* ::Package:: *)

(* ::Section:: *)
(*All*)


(* ::Subsubsection:: *)
(*setup*)


(*SetDirectory[NotebookDirectory[]]*)


SetDirectory[Directory[]<>"/fermionTWA"];


<<randomSeed.wl


<<dynComplex.wl


(*<<2dfuncs.wl*)


<<constFermVel1d.wl


<<eqsTwoChannel.wl


<<initsComplex.wl


<<initsDisc.wl


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


mmu=MaxMemoryUsed[]/10.^6;


SetDirectory[ParentDirectory[]];


Save["dataFermion.dat",{mmu,t1,t2,occupied,fermOc,boseOc}];
