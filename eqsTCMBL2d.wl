(* ::Package:: *)

makeDSolveStart=Function[{observables},
Block[{
hamkinb=Total[(bh[#][t]\[Conjugate]bh[#][t]-1/2)&/@Range[numbos]],
hamkinf=Total[(hEm[#1,#2]+hEm[#2,#1]+hEm[#1+sites,#2+sites]+hEm[#2+sites,#1+sites])&@@@bonds],
hamkinfPerp=Total[(hEm[#1,#2]+hEm[#2,#1]+hEm[#1+sites,#2+sites]+hEm[#2+sites,#1+sites])&@@@bondsPerp],
(*hamdisf=Total[dis[[cfneither[#][[3]]+1]](hEm[#,#]+hEm[#+sites,#+sites])&/@Range[sites]],
*)
hamdisf=Total[dis[[#]](hEm[#,#]+hEm[#+sites,#+sites])&/@Range[sites]],
hamint=Total[(bh[#][t]hEu[#,#+sites]+bh[#][t]\[Conjugate]hEl[#+sites,#])&/@Range[sites]],
hamtotb,beqns,binits,eqnsEm,eqnsEl,initsEm,initsEl,start
},
hamtotb=\[Omega][t]hamkinb+g[t]hamint[[1]];
beqns=Table[bdot[bh[nn],hamtotb],{nn,numbos}];
binits=Table[bh[nn][0]==0,{nn,numbos}];
eqnsEm=Em[#1,#2]'[t]==-hamkinf[[2,1,#1,#2]]-jPerp hamkinfPerp[[2,1,#1,#2]]+hamdisf[[2,1,#1,#2]]+g[t]hamint[[2,1,#1,#2]]&@@@midPairs;
eqnsEl=El[#1,#2]'[t]==-hamkinf[[2,2,#1,#2]]-jPerp hamkinfPerp[[2,2,#1,#2]]+hamdisf[[2,2,#1,#2]]+g[t]hamint[[2,2,#1,#2]]&@@@lowPairs;
initsEm=Em[#1,#2][0]==0&@@@midPairs;
initsEl=El[#1,#2][0]==0&@@@lowPairs;
start=First@NDSolve`ProcessEquations[Flatten[{beqns,eqnsEm,eqnsEl,binits,initsEm,initsEl}],observables,t,Method->{"EquationSimplification"->"Solve"}];
start
]
];
