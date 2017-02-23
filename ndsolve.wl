(* ::Package:: *)

singleRun=Function[{startEq,newInits,obfun},
Block[{newstate=First@NDSolve`Reinitialize[startEq,newInits],sol,values},
NDSolve`Iterate[newstate,tmax];
sol=NDSolve`ProcessSolutions[newstate][[All,2]];
values=(Outer[Through[#1[#2]]&,sol,times,1]);
Chop[obfun[values]]
]
];


singleRunShort=Function[{startEq,newInits,times},
Block[{newstate=First@NDSolve`Reinitialize[startEq,newInits],sol,values},
NDSolve`Iterate[newstate,Last[times]];
sol=NDSolve`ProcessSolutions[newstate][[All,2]];
values=Outer[#2[#1]&,times,sol]
]
];


randomInitsFermiHubbard:=Flatten[{Em[#1,#2][0]==randomEm[#1,#2]&@@@midPairs,El[#1,#2][0]==randomEl[#1,#2]&@@@lowPairs}]


randomInitsFermiHubbardDisc:=Flatten[{Em[#1,#2][0]==discRandomEm[#1,#2]&@@@midPairs,El[#1,#2][0]==discRandomEl[#1,#2]&@@@lowPairs}]


randomInitsFermiHubbardWide:=Flatten[{Em[#1,#2][0]==randomEmWide[#1,#2]&@@@midPairs,El[#1,#2][0]==randomElWide[#1,#2]&@@@lowPairs}]


randomInitsFermiHubbardPlus:=Flatten[{Em[#1,#2][0]==randomEmPlus[#1,#2]&@@@midPairs,El[#1,#2][0]==randomElPlus[#1,#2]&@@@lowPairs}]


meanInitsFermiHubbard:=Flatten[{Em[#1,#2][0]==meanEm[#1,#2]&@@@midPairs,El[#1,#2][0]==meanEl[#1,#2]&@@@lowPairs}]


randomInitsFHMid[st_,obs_] := Flatten[{Em[#1,#2][st]==obs[[1,Position[midPairs,{#1,#2}][[1,1]]]]&@@@midPairs,El[#1,#2][st]==obs[[2,Position[lowPairs,{#1,#2}][[1,1]]]]&@@@lowPairs}]


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


runMeanInitsFermiHubbard=Function[{startEq,obfun},
Block[{initsEm,initsEl},
initsEm=Em[#1,#2][0]==meanEm[#1,#2]&@@@midPairs;
initsEl=El[#1,#2][0]==meanEl[#1,#2]&@@@lowPairs;
singleRun[startEq,Flatten[{initsEm,initsEl}],obfun]
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


runRandomInitsFermiHubbardPlus=Function[{startEq,obfun},
Block[{initsEm,initsEl},
initsEm=Em[#1,#2][0]==randomEmPlus[#1,#2]&@@@midPairs;
initsEl=El[#1,#2][0]==randomElPlus[#1,#2]&@@@lowPairs;
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
