(* ::Package:: *)

uup=0.35;
x0=0.1;
int[n_,m_,p_,q_]:=If[(n<0)\[Or](m<0)\[Or](p<0)\[Or](q<0),0,If[EvenQ[n+m+p+q],((Sqrt[m!] Sqrt[n!])/(Sqrt[2] \[Pi]^2 Sqrt[p!] Sqrt[q!]))*Sum[(Gamma[1/2 (1+m+n+p-q-2 t)] Gamma[1/2 (1+m+n-p+q-2 t)] Gamma[1/2 (1-m-n+p+q)+t])/( (m-t)! (n-t)! t!),{t,0,Min[n,m]}],0]];
choose=Function[{a,b,c,d},
list={{Abs[a-b],Abs[c-d]},{Abs[a-d],Abs[b-c]}};
If[Min[list[[1]]]<Min[list[[2]]],1,
If[Min[list[[1]]]>Min[list[[2]]],2,
If[Max[list[[1]]]<Max[list[[2]]],1,
If[Max[list[[1]]]>Max[list[[2]]],2,
3
]
]
]
]
]
hamFunc[1,a_,b_,c_,d_]:=((vEm[#1,#2]+KroneckerDelta[#1,#2]/2)hEm[#3+sites,#4+sites]+hEm[#1,#2](vEm[#3+sites,#4+sites]+KroneckerDelta[#3,#4]/2))&[a,b,c,d]
hamFunc[2,a_,b_,c_,d_]:=(-(vEm[#1,#4+sites]hEm[#3+sites,#2]+hEm[#1,#4+sites]vEm[#3+sites,#2])
+KroneckerDelta[#3,#4]hEm[#1,#2]/2
+KroneckerDelta[#1,#2]hEm[#3+sites,#4+sites]/2)&[a,b,c,d]
hamFunc[3,a_,b_,c_,d_]:=(hamFunc[1,a,b,c,d]+hamFunc[2,a,b,c,d])/2
makeDSolveStartFermiHubbard=Function[{observables},
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
(
(1-KroneckerDelta[#1,#3]/2)(KroneckerDelta[#1,#2]KroneckerDelta[#3,#4]+KroneckerDelta[#1,#4]KroneckerDelta[#3,#2])hamFunc[choose[#1,#2,#3,#4],#1,#2,#3,#4]
)
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
