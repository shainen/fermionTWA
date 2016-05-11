(* ::Package:: *)

(* ::Section:: *)
(*All*)


(* ::Subsubsection:: *)
(*setup*)


SetDirectory[NotebookDirectory[]]


(*SetDirectory[Directory[]<>"/fermionTWA"];*)


<<randomSeed.wl


<<dynComplex.wl


<<2dfuncs.wl


<<constTwoChannel.wl


<<eqsTwoChannel.wl


<<initsComplexMom.wl


<<ndsolve.wl


(* ::Subsection:: *)
(*run TWA*)


Dynamic[rr]


(*observables={Em[#,#]&/@Range[numferm],bh[#]&/@Range[numbos],
Flatten[{Table[Table[Em[ii,jj],{jj,ii+1,numferm}],{ii,numferm-1}],Table[Table[El[ii,jj],{jj,ii+1,numferm}],{ii,numferm-1}]}]};
*)
observables=Join[{bh[#]&/@Range[numbos]},
Table[Table[Em[ii,jj],{jj,ii,numferm}],{ii,numferm}](*,Table[Table[El[ii,jj],{jj,ii+1,numferm}],{ii,numferm-1}]*)];
obsfun=Function[{values},
Join[{Abs[InverseFourier[Partition[#,length]]]^2&/@(values[[1]])-1/2},values[[2;;1+numferm]]]
];


t1=Timing[start=makeDSolveStart[observables];];


fullTWA=0;
t2=Timing[Table[
fullTWA+=singleRun[start,initsMom,obsfun]/runs;
,{rr,runs}];];


nbos=Transpose[fullTWA[[1]],{3,1,2}];


matDataEm:=fullTWA[[2;;1+numferm]]\[Transpose];
(*matDataEl:=fullTWA[[1+numferm+1;;1+2numferm-1]]\[Transpose];*)


listToMatEm[list_,i_,j_]:=If[i<=j,list[[i,j-i+1]],list[[j,i-j+1]]\[Conjugate]]
(*listToMatEl[list_,i_,j_]:=Which[i==j,0,i\[LessEqual]j,list[[i,j-i]],True,-list[[j,i-j]]]*)


momNums[data_]:=Diagonal[makeMom[Table[listToMatEm[data,i,j],{i,numferm},{j,numferm}],Fourier,InverseFourier]]+.5


t3=Timing[fermMomNums=(momNums/@matDataEm)\[Transpose];];


mmu=MaxMemoryUsed[]/10.^6;


SetDirectory[ParentDirectory[]];


Save["dataFermion.dat",{mmu,t1,t2,t3,nbos,fermMomNums}];
