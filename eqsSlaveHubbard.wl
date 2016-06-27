(* ::Package:: *)

makeDSolveStart=Function[{observables},
Block[{
hamkinb=Total[(bh[#][t]\[Conjugate]bh[#][t]-1/2)&/@Range[sites+1,2sites]],
hamIntAndFEqs=Total[( 
(hEm[#1,#2]+hEm[#1+sites,#2+sites])
(bh[#2][t]\[Conjugate]bh[#1][t]-bh[#2+sites][t]\[Conjugate]bh[#1+sites][t])
+(hEu[#1,#2+sites]-hEu[#1+sites,#2])
(bh[#1][t]bh[#2+sites][t]+bh[#1+sites][t]bh[#2][t])
+(hEm[#2,#1]+hEm[#2+sites,#1+sites])
(bh[#2][t]bh[#1][t]\[Conjugate]-bh[#2+sites][t]bh[#1+sites][t]\[Conjugate])
+(hEl[#2+sites,#1]-hEl[#2,#1+sites])
(bh[#1][t]\[Conjugate]bh[#2+sites][t]\[Conjugate]+bh[#1+sites][t]\[Conjugate]bh[#2][t]\[Conjugate])
)&@@@bonds],
hamtotb,beqns,binits,eqnsEm,eqnsEl,initsEm,initsEl,start
},
hamtotb=intU[t]hamkinb-hopt[t]hamIntAndFEqs[[1]];
beqns=Table[bdot[bh[nn],hamtotb],{nn,numbos}];
binits=Table[bh[nn][0]==0,{nn,numbos}];
eqnsEm=Em[#1,#2]'[t]==-hopt[t]hamIntAndFEqs[[2,1,#1,#2]]&@@@midPairs;
eqnsEl=El[#1,#2]'[t]==-hopt[t]hamIntAndFEqs[[2,2,#1,#2]]&@@@lowPairs;
initsEm=Em[#1,#2][0]==0&@@@midPairs;
initsEl=El[#1,#2][0]==0&@@@lowPairs;
start=First@NDSolve`ProcessEquations[Flatten[{beqns,eqnsEm,eqnsEl,binits,initsEm,initsEl}],observables,t,Method->{"EquationSimplification"->"Solve"}];
start
]
];
