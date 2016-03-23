(* ::Package:: *)

singleRun=Function[{startEq,newInits,obfun},
Block[{newstate=First@NDSolve`Reinitialize[startEq,newInits],sol,values},
NDSolve`Iterate[newstate,tmax];
sol=NDSolve`ProcessSolutions[newstate][[All,2]];
values=(Outer[Through[#1[#2]]&,sol,times,1]);
Chop[obfun[values]]
]
];


runRandomInits=Function[{startEq,obfun},
Block[{initsb,initsEm,initsEl},
initsb=MapThread[Equal,{Table[bh[ii][0],{ii,numbos}],randomBoseInits}];
initsEm=Apply[Equal,(Transpose/@({Table[Table[Em[ii,jj][0],{jj,ii,numferm}],{ii,numferm}],randomEm}\[Transpose])),{2}];
initsEl=Apply[Equal,(Transpose/@({Table[Table[El[ii,jj][0],{jj,ii+1,numferm}],{ii,numferm-1}],randomEl}\[Transpose])),{2}];
singleRun[startEq,Flatten[{initsb,initsEm,initsEl}],obfun]
]
];
