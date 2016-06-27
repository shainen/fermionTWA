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


runMomInits=Function[{startEq,obfun},
Block[{initsEm,initsEl,initsb},
initsb=MapThread[Equal,{Table[bh[ii][0],{ii,numbos}],randomBoseInits}];
singleRun[startEq,Flatten[{initsEmMom,initsElMom,initsb}],obfun]
]
];


runDiscRandomInits=Function[{startEq,obfun},
Block[{initsEm,initsEl,initsb},
initsb=MapThread[Equal,{Table[bh[ii][0],{ii,numbos}],randomBoseInits}];
initsEm=Em[#1,#2][0]==discRandomEm[#1,#2]&@@@midPairs;
initsEl=El[#1,#2][0]==discRandomEl[#1,#2]&@@@lowPairs;
singleRun[startEq,Flatten[{initsEm,initsEl,initsb}],obfun]
]
];


runDiscSingleBoseInits=Function[{startEq,obfun},
Block[{initsEm,initsEl,initsb},
initsb=bh[#][0]==discRandomSb[#]&/@Range[numbos];
initsEm=Em[#1,#2][0]==discRandomEm[#1,#2]&@@@midPairs;
initsEl=El[#1,#2][0]==discRandomEl[#1,#2]&@@@lowPairs;
singleRun[startEq,Flatten[{initsEm,initsEl,initsb}],obfun]
]
];


runDiscSHHC=Function[{startEq,obfun},
Block[{initsEm,initsEl,initsSm,initsSz},
initsSm=Sm[#][0]==discRandomSm[#]&/@Range[numbos];
initsSz=Sz[#][0]==discRandomSz[#]&/@Range[numbos];
initsEm=Em[#1,#2][0]==discRandomEm[#1,#2]&@@@midPairs;
initsEl=El[#1,#2][0]==discRandomEl[#1,#2]&@@@lowPairs;
singleRun[startEq,Flatten[{initsEm,initsEl,initsSm,initsSz}],obfun]
]
];


runDiscSHHCMmom=Function[{startEq,obfun},
Block[{initsEm,initsEl,initsSm,initsSz},
initsSz=Sz[#][0]==discRandomSz[#]&/@Range[numbos];
singleRun[startEq,Flatten[{initsEmMom,initsElMom,initsSmMom,initsSz}],obfun]
]
];


(*runDiscSHHC2siteMom=Function[{startEq,obfun},
Block[{initsEm,initsEl,initsSm,initsSz},
ops={
{{},{2,3}},
{{},{1,4}},
{{1,4},{}},
{{2,3},{}}
};
choice=RandomChoice[ops];
occupied=choice[[1]];
occupiedB=choice[[2]];
initsSm=Sm[#][0]==discRandomSm[#]&/@Range[numbos];
initsSz=Sz[#][0]==discRandomSz[#]&/@Range[numbos];
initsEm=Em[#1,#2][0]==discRandomEm[#1,#2]&@@@midPairs;
initsEl=El[#1,#2][0]==discRandomEl[#1,#2]&@@@lowPairs;
singleRun[startEq,Flatten[{initsEm,initsEl,initsSm,initsSz}],obfun]
]
];*)


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


runMomInitsFermiHubbard=Function[{startEq,obfun},
singleRun[startEq,Flatten[{initsEmMom,initsElMom}],obfun]
];


runDistInitsFermiHubbard=Function[{startEq,obfun},
Block[{initsEm,initsEl},
initsEm=Em[#1,#2][0]==discRandomEm[#1,#2]&@@@midPairs;
initsEl=El[#1,#2][0]==discRandomEl[#1,#2]&@@@lowPairs;
singleRun[startEq,Flatten[{initsEm,initsEl}],obfun]
]
];


runRandomInitsCDisc=Function[{startEq,obfun},
Block[{initsEm,initsEl,initsBm,initsBl},
initsEm=Em[#1,#2][0]==discRandomEm[#1,#2]&@@@midPairs;
initsEl=El[#1,#2][0]==discRandomEl[#1,#2]&@@@lowPairs;
initsBm=Bm[#1,#2][0]==discRandomBm[#1,#2]&@@@midPairs;
initsBl=Bl[#1,#2][0]==discRandomBl[#1,#2]&@@@lowPairs;
singleRun[startEq,Flatten[{initsEm,initsEl,initsBm,initsBl}],obfun]
]
];
