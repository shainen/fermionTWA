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


<<constFermiHubbardVel2d.wl


<<eqsFermiHubbard.wl


<<initsComplex.wl


<<initsDisc.wl


<<ndsolve.wl


(* ::Subsection:: *)
(*run TWA*)


Dynamic[rr]


(*observables={Em[#,#]&/@Range[numferm],bh[#]&/@Range[numbos]
(*,Flatten[{Table[Table[Em[ii,jj],{jj,ii+1,numferm}],{ii,numferm-1}],Table[Table[El[ii,jj],{jj,ii+1,numferm}],{ii,numferm-1}]}]*)
};
obsfun=Function[{values},
{values[[1]]+1/2,Abs[values[[2]]]^2-1/2,
(*Total[(values[[1]]\[Transpose])^2]/2+Total[Abs[values[[3]]\[Transpose]]^2]*)
values[[2]]
}
];*)


observables={Em[#,#]&/@Range[numferm]
(*,Flatten[{Em[#1,#2]&@@@midPairs,El[#1,#2]&@@@lowPairs}]*)
};
obsfun=Function[{values},
{values[[1]]+1/2(*,values[[2]]Total[(values[[1]]\[Transpose])^2]/2+Total[Abs[values[[3]]\[Transpose]]^2]*)}
];


(*start=makeDSolveStart[observables];*)


start=makeDSolveStartFermiHubbard[observables];


(*eachTWA={};
Table[
AppendTo[eachTWA,runRandomInitsFermiHubbard[start,obsfun]];
,{rr,runs}];
fullTWA=Total[eachTWA]/runs;*)


fullTWA=0;
t2=Table[
Timing[
fullTWA+=runRandomInitsFermiHubbard[start,obsfun]/runs;
][[1]]
,{rr,runs}];


fermOc=fullTWA[[1]]\[Transpose];


(*fnumTWA=ArrayReshape[fullTWA[[1]]\[Transpose],{2,sites,steps}]\[Transpose];*)


(*kinTermTWA=Total[2Re[fullTWA[[2]]\[Transpose][[{2,5}]]]];*)


(*eachTWAC={};
Table[
AppendTo[eachTWAC,runRandomInitsC[startC,obsfun2]/runs];
,{rr,runs}];
fullTWAC=Total[eachTWAC];*)


mmu=MaxMemoryUsed[]/10.^6;


SetDirectory[ParentDirectory[]];


Save["dataFermion.dat",{mmu,fermOc}];
