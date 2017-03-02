(* ::Package:: *)

(* ::Section:: *)
(*All*)


(* ::Subsubsection:: *)
(*setup*)


(*SetDirectory[NotebookDirectory[]]*)


SetDirectory[Directory[]<>"/fermionTWA"];


SetSystemOptions["ParallelOptions" -> "ParallelThreadNumber" -> 1];
SetSystemOptions["MKLThreads" -> 1];


<<randomSeed.wl


<<2dfuncs.wl


<<dynComplex.wl


<<constSYK.wl


<<eqsSYKboth.wl


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


observables={Em[#1,#2]&@@@midPairs,El[#1,#2]&@@@lowPairs
(*Em[#,#]&/@Range[numferm]*)
(*,Flatten[{Em[#1,#2]&@@@midPairs,El[#1,#2]&@@@lowPairs}]*)
};
obsfun=Function[{values},
{values[[1,Position[midPairs,{#,#}][[1,1]]]]+1/2&/@Range[numferm],values[[1]](*,values[[2]]Total[(values[[1]]\[Transpose])^2]/2+Total[Abs[values[[3]]\[Transpose]]^2]*)}
];


(*start=makeDSolveStart[observables];*)


(*real=Import["/projectnb/twambl/170207_1_s16f4/flatJreal.CSV","Data"];
imag=Import["/projectnb/twambl/170207_1_s16f4/flatJimag.CSV","Data"];
complex=real+I*imag;
Jcoup=ArrayReshape[complex,{sites,sites,sites,sites}];*)


(*Timing[start=makeDSolveStartSYK[observables];]*)


t1=Timing[Get[pathToStart]];


(*eachTWA={};
Table[
AppendTo[eachTWA,runRandomInitsFermiHubbard[start,obsfun]];
,{rr,runs}];
fullTWA=Total[eachTWA]/runs;*)


Timing[
fullTWA=0;
firstTime=First@splitTimes;
nextTimes=Drop[splitTimes,1];
Table[
t2=Timing[stuff=singleRunShort[start,randomInitsFermiHubbard,firstTime];];
(*stuff=singleRunShort[start,meanInitsFermiHubbard,firstTime];*)
lastTime=Last@firstTime;
t3={};
Table[
AppendTo[t3,Timing[stuff=Join[stuff,singleRunShort[start,Flatten[randomInitsFHMid[lastTime,Last@stuff]],trange]];]];
lastTime=Last@trange;
,{trange,nextTimes}];
t4=Timing[newObs=Chop[obsfun/@stuff];];
t5=Timing[AddTo[fullTWA,newObs/runs];];
,{rr,runs}];]


t6=Timing[fermOc=fullTWA[[All,1]]\[Transpose];];


t6=Timing[expCorrs=fullTWA[[All,2]]\[Transpose];];


mmu=MaxMemoryUsed[]/10.^6;


SetDirectory[ParentDirectory[]];


Save["dataFermion.dat",{mmu,t1,t2,t3,t4,t5,t6,fermOc,expCorrs}];
