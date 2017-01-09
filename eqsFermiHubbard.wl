(* ::Package:: *)

makeDSolveStartFermiHubbard=Function[{observables},
Block[{
hamKin=Total[1/distance[#1,#2]^alpha (
hEm[#1,#2]+hEm[#1+sites,#2+sites]
+hEm[#2,#1]+hEm[#2+sites,#1+sites]
)&@@@bonds],
hamInt=Total[(
sym1*((vEm[#,#]+KroneckerDelta[#,#]/2)hEm[#+sites,#+sites]+hEm[#,#](vEm[#+sites,#+sites]+KroneckerDelta[#,#]/2))
+sym2*(-(vEm[#,#+sites]hEm[#+sites,#]+hEm[#,#+sites]vEm[#+sites,#])
+KroneckerDelta[#,#]hEm[#,#]/2
+KroneckerDelta[#,#]hEm[#+sites,#+sites]/2)
+sym3*(-(vEu[#,#+sites]hEl[#,#+sites]+hEu[#,#+sites]vEl[#,#+sites])
+KroneckerDelta[#,#]hEm[#+sites,#+sites]/2
+KroneckerDelta[#,#]hEm[#,#]/2)
)&/@Range[sites]],
eqnsEm,eqnsEl,initsEm,initsEl,start
},
eqnsEm=Em[#1,#2]'[t]==(-hopt[t]hamKin+intU[t]hamInt)[[2,1,#1,#2]]&@@@midPairs;
eqnsEl=El[#1,#2]'[t]==(-hopt[t]hamKin+intU[t]hamInt)[[2,2,#1,#2]]&@@@lowPairs;
initsEm=Em[#1,#2][0]==0&@@@midPairs;
initsEl=El[#1,#2][0]==0&@@@lowPairs;
start=First@NDSolve`ProcessEquations[Flatten[{eqnsEm,eqnsEl,initsEm,initsEl}],observables,t,Method->{"EquationSimplification"->"Solve"}];
start
]
];
