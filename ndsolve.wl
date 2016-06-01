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
Block[{initsEm,initsEl,initsb},
initsb=MapThread[Equal,{Table[bh[ii][0],{ii,numbos}],randomBoseInits}];
initsEm=Em[#1,#2][0]==randomEm[#1,#2]&@@@midPairs;
initsEl=El[#1,#2][0]==randomEl[#1,#2]&@@@lowPairs;
singleRun[startEq,Flatten[{initsEm,initsEl,initsb}],obfun]
]
];


runMeanInits=Function[{startEq,obfun},
Block[{initsEm,initsEl,initsb},
initsb=bh[#][0]==coh[[#]]&/@Range[numbos];
initsEm=Em[#1,#2][0]==meanEm[#1,#2]&@@@midPairs;
initsEl=El[#1,#2][0]==meanEl[#1,#2]&@@@lowPairs;
singleRun[startEq,Flatten[{initsEm,initsEl,initsb}],obfun]
]
];


runRFMBInits=Function[{startEq,obfun},
Block[{initsEm,initsEl,initsb},
initsb=bh[#][0]==coh[[#]]&/@Range[numbos];
initsEm=Em[#1,#2][0]==randomEm[#1,#2]&@@@midPairs;
initsEl=El[#1,#2][0]==randomEl[#1,#2]&@@@lowPairs;
singleRun[startEq,Flatten[{initsEm,initsEl,initsb}],obfun]
]
];


runRandomInitsFermiHubbard=Function[{startEq,obfun},
Block[{initsEm,initsEl},
initsEm=Em[#1,#2][0]==randomEm[#1,#2]&@@@midPairs;
initsEl=El[#1,#2][0]==randomEl[#1,#2]&@@@lowPairs;
singleRun[startEq,Flatten[{initsEm,initsEl}],obfun]
]
];


runRandomInitsC=Function[{startEq,obfun},
Block[{initsEm,initsEl,initsBm,initsBl},
(*initsb=MapThread[Equal,{Table[bh[ii][0],{ii,numbos}],randomBoseInits}];*)
(*initsEm=Em[#1,#2][0]==randomEm[#1,#2]&@@@midPairs;
initsEl=El[#1,#2][0]==randomEl[#1,#2]&@@@lowPairs;*)
initsEm=startEm;
initsEl=startEl;
initsBm=Bm[#1,#2][0]==randomBm[#1,#2]&@@@midPairs;
initsBl=Bl[#1,#2][0]==randomBl[#1,#2]&@@@lowPairs;
(*initsBm=Bm[#1,#2][0]\[Equal]startbs[[#1]]\[Conjugate]startbs[[#2]]&@@@midPairs;
initsBl=Bl[#1,#2][0]==startbs[[#1]]startbs[[#2]]&@@@lowPairs;*)
singleRun[startEq,Flatten[{initsEm,initsEl,initsBm,initsBl}],obfun]
]
];
