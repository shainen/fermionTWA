(* ::Package:: *)

makeDSolveStart=Function[{observables},
Block[{
hamkinb=Total[(bh[#][t]\[Conjugate]bh[#][t]-1/2)&/@{3,4}],
hamIntAndFEqs=Total[( 
(hEm[#1,#2]+hEm[#1+sites,#2+sites])
(bh[#2][t]\[Conjugate]bh[#1][t]-bh[#2+sites][t]\[Conjugate]bh[#1+sites][t])
+(hEu[#1,#2+sites]-hEu[#1+sites,#2])
(bh[#1][t]bh[#2+sites][t]+bh[#1+sites][t]bh[#2][t])
+(hEm[#2,#1]+hEm[#2+sites,#1+sites])
(bh[#2][t]bh[#1][t]\[Conjugate]-bh[#2+sites][t]bh[#1+sites][t]\[Conjugate])
+(hEl[#2+sites,#1]-hEl[#2,#1+sites])
(bh[#1][t]\[Conjugate]bh[#2+sites][t]\[Conjugate]+bh[#1+sites][t]\[Conjugate]bh[#2][t]\[Conjugate])
)&@@@Table[{n,Mod[n+1,length,1]},{n,length}]],
hamtot,beqns,binits,eqnsEm,eqnsEl,initsEm,initsEl,start
},
hamtot=intU[t]hamkinb-hopt[t]hamIntAndFEqs[[1]];
beqns=Table[bdot[bh[nn],hamtot],{nn,numbos}];
binits=Table[bh[nn][0]==0,{nn,numbos}];
eqnsEm=Table[Table[Em[ii,jj]'[t]==-hopt[t]hamIntAndFEqs[[2,1,ii,jj]],{jj,ii,numferm}],{ii,numferm}];
eqnsEl=Table[Table[El[ii,jj]'[t]==-hopt[t]hamIntAndFEqs[[2,2,ii,jj]],{jj,ii+1,numferm}],{ii,numferm-1}];
initsEm=Table[Table[Em[ii,jj][0]==0,{jj,ii,numferm}],{ii,numferm}];
initsEl=Table[Table[El[ii,jj][0]==0,{jj,ii+1,numferm}],{ii,numferm-1}];
start=First@NDSolve`ProcessEquations[Flatten[{beqns,eqnsEm,eqnsEl,binits,initsEm,initsEl}],observables,t,Method->{"EquationSimplification"->"Solve"}];
start
]
];
