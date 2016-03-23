(* ::Package:: *)

makeDSolveStart=Function[{observables},
Block[{
hamkinb=Total[(bh[#][t]\[Conjugate]bh[#][t]-1/2)&/@Range[numbos]],
hamkinf=Total[(hEm[#1,#2]+hEm[#2,#1])&@@@bonds],
hamint=Total[(bh[#][t]hEu[#,#+sites]+bh[#][t]\[Conjugate]hEl[#+sites,#])&/@Range[sites]],
hamtot,beqns,binits,eqnsEm,eqnsEl,initsEm,initsEl,start
},
hamtot=\[Omega][t]hamkinb-hamkinf[[1]]+g[t]hamint[[1]];
beqns=Table[bdot[bh[nn],hamtot],{nn,numbos}];
binits=Table[bh[nn][0]==0,{nn,numbos}];
eqnsEm=Table[Table[Em[ii,jj]'[t]==-hamkinf[[2,1,ii,jj]]+g[t]hamint[[2,1,ii,jj]],{jj,ii,numferm}],{ii,numferm}];
eqnsEl=Table[Table[El[ii,jj]'[t]==-hamkinf[[2,2,ii,jj]]+g[t]hamint[[2,2,ii,jj]],{jj,ii+1,numferm}],{ii,numferm-1}];
initsEm=Table[Table[Em[ii,jj][0]==0,{jj,ii,numferm}],{ii,numferm}];
initsEl=Table[Table[El[ii,jj][0]==0,{jj,ii+1,numferm}],{ii,numferm-1}];
start=First@NDSolve`ProcessEquations[Flatten[{beqns,eqnsEm,eqnsEl,binits,initsEm,initsEl}],observables,t,Method->{"EquationSimplification"->"Solve"}];
start
]
];
