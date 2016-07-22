(* ::Package:: *)

makeDSolveStartFermiHubbard=Function[{observables},
uup=0.35;
x0=0.1;
int[n_,m_,p_,q_]:=If[(n<0)\[Or](m<0)\[Or](p<0)\[Or](q<0),0,If[EvenQ[n+m+p+q],((Sqrt[m!] Sqrt[n!])/(Sqrt[2] \[Pi]^2 Sqrt[p!] Sqrt[q!]))*Sum[(Gamma[1/2 (1+m+n+p-q-2 t)] Gamma[1/2 (1+m+n-p+q-2 t)] Gamma[1/2 (1-m-n+p+q)+t])/( (m-t)! (n-t)! t!),{t,0,Min[n,m]}],0]];
Block[{
hamKin=Total[(
(#-1/2)(hEm[#,#]+hEm[#+sites,#+sites])
)&/@Range[sites]],
hamShift=Total[(
x0 Sqrt[#-1]/Sqrt[2](hEm[#,#-1]-hEm[#+sites,#-1+sites]+hEm[#-1,#]-hEm[#-1+sites,#+sites])
)&/@Range[2,sites]],
(*hamInt=Total[( 
int[#1-1,#2-1,#3-1,#4-1]
(-vEm[#1,#2+sites]hEm[#3+sites,#4]-hEm[#1,#2+sites]vEm[#3+sites,#4]
+KroneckerDelta[#2,#3]hEm[#1,#4]/2
+KroneckerDelta[#1,#4]hEm[#3+sites,#2+sites]/2)
)&@@@Tuples[Range[sites],4]],*)
hamInt=Total[( 
int[#1-1,#2-1,#3-1,#4-1]
((vEm[#1,#2]hEm[#3+sites,#4+sites]+hEm[#1,#2]vEm[#3+sites,#4+sites])/3
+2(-(vEm[#1,#2+sites]hEm[#3+sites,#4]+hEm[#1,#2+sites]vEm[#3+sites,#4])
+KroneckerDelta[#2,#3]hEm[#1,#4]/2
+KroneckerDelta[#1,#4]hEm[#3+sites,#2+sites]/2)/3)
)&@@@Tuples[Range[sites],4]],
eqnsEm,eqnsEl,initsEm,initsEl,start
},
eqnsEm=Em[#1,#2]'[t]==(hamKin+hamShift+uup hamInt)[[2,1,#1,#2]]&@@@midPairs;
eqnsEl=El[#1,#2]'[t]==(hamKin+hamShift+uup hamInt)[[2,2,#1,#2]]&@@@lowPairs;
initsEm=Em[#1,#2][0]==0&@@@midPairs;
initsEl=El[#1,#2][0]==0&@@@lowPairs;
start=First@NDSolve`ProcessEquations[Flatten[{eqnsEm,eqnsEl,initsEm,initsEl}],observables,t,Method->{"EquationSimplification"->"Solve"}];
start
]
];
