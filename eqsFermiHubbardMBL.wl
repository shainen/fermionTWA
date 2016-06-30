(* ::Package:: *)

makeDSolveStartFermiHubbard=Function[{observables},
Block[{
hamKin=Total[(
hEm[#1,#2]+hEm[#1+sites,#2+sites]
+hEm[#2,#1]+hEm[#2+sites,#1+sites]
)&@@@bonds],
hamInt=Total[( 
vEm[#,#]hEm[#+sites,#+sites]+hEm[#,#]vEm[#+sites,#+sites]
)&/@Range[sites]],
hamdisf=Total[dis[[#]](hEm[#,#]+hEm[#+sites,#+sites])&/@Range[sites]],
eqnsEm,eqnsEl,initsEm,initsEl,start
},
eqnsEm=Em[#1,#2]'[t]==(-hopt[t]hamKin+intU[t]hamInt+hamdisf)[[2,1,#1,#2]]&@@@midPairs;
eqnsEl=El[#1,#2]'[t]==(-hopt[t]hamKin+intU[t]hamInt+hamdisf)[[2,2,#1,#2]]&@@@lowPairs;
initsEm=Em[#1,#2][0]==0&@@@midPairs;
initsEl=El[#1,#2][0]==0&@@@lowPairs;
start=First@NDSolve`ProcessEquations[Flatten[{eqnsEm,eqnsEl,initsEm,initsEl}],observables,t,Method->{"EquationSimplification"->"Solve"}];
start
]
];
