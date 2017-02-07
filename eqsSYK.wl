(* ::Package:: *)

makeDSolveStartSYK=Function[{observables},
Block[{
hamInt=Total[(Jcoup[[#1,#2,#3,#4]](
-(
(vEm[#1,#3]+KroneckerDelta[#1,#3]/2)hEm[#2,#4]
+hEm[#1,#3](vEm[#2,#4]+KroneckerDelta[#2,#4]/2)
+KroneckerDelta[#2,#3]hEm[#1,#4]/2
-KroneckerDelta[#1,#4]hEm[#2,#3]/2
)
+KroneckerDelta[#2,#3]hEm[#1,#4]
)
)&@@@Tuples[Range[sites],4]],
eqnsEm,eqnsEl,initsEm,initsEl,start
},
eqnsEm=Em[#1,#2]'[t]==hamInt[[2,1,#1,#2]]&@@@midPairs;
eqnsEl=El[#1,#2]'[t]==hamInt[[2,2,#1,#2]]&@@@lowPairs;
initsEm=Em[#1,#2][0]==0&@@@midPairs;
initsEl=El[#1,#2][0]==0&@@@lowPairs;
start=First@NDSolve`ProcessEquations[Flatten[{eqnsEm,eqnsEl,initsEm,initsEl}],observables,t,Method->{"EquationSimplification"->"Solve"}];
start
]
];
