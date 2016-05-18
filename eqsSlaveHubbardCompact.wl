(* ::Package:: *)

makeDSolveStartCompact=Function[{observables},
Block[{
hamkinb=Total[(hBm[#,#])&/@Range[length+1,2length]],
hamIntAndFEqs=Total[( 
(hEm[#1,#2]+hEm[#1+sites,#2+sites])
(hBm[#2,#1]-hBm[#2+sites,#1+sites])
+(hEu[#1,#2+sites]-hEu[#1+sites,#2])
(hBl[#1,#2+sites]+hBl[#1+sites,#2])
+(hEm[#2,#1]+hEm[#2+sites,#1+sites])
(hBm[#1,#2]-hBm[#1+sites,#2+sites])
+(hEl[#2+sites,#1]-hEl[#2,#1+sites])
(hBu[#1,#2+sites]+hBu[#1+sites,#2])
)&@@@bonds],
eqnsBm,eqnsBl,initsBm,initsBl,eqnsEm,eqnsEl,initsEm,initsEl,start
},
eqnsBm=Bm[#1,#2]'[t]==(intU[t]hamkinb-hopt[t]hamIntAndFEqs)[[1,1,#1,#2]]&@@@midPairs;
eqnsBl=Bl[#1,#2]'[t]==(intU[t]hamkinb-hopt[t]hamIntAndFEqs)[[1,2,#1,#2]]&@@@lowPairs;
initsBm=Bm[#1,#2][0]==0&@@@midPairs;
initsBl=Bl[#1,#2][0]==0&@@@lowPairs;
eqnsEm=Em[#1,#2]'[t]==-hopt[t]hamIntAndFEqs[[2,1,#1,#2]]&@@@midPairs;
eqnsEl=El[#1,#2]'[t]==-hopt[t]hamIntAndFEqs[[2,2,#1,#2]]&@@@lowPairs;
initsEm=Em[#1,#2][0]==0&@@@midPairs;
initsEl=El[#1,#2][0]==0&@@@lowPairs;
start=First@NDSolve`ProcessEquations[Flatten[{eqnsBm,eqnsBl,eqnsEm,eqnsEl,initsBm,initsBl,initsEm,initsEl}],observables,t,Method->{"EquationSimplification"->"Solve"}];
start
]
];