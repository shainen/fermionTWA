(* ::Package:: *)

vBm[xx_,yy_]:=Which[yy<xx,Bm[yy,xx][t]\[Conjugate],True,Bm[xx,yy][t]]
vBl[xx_,yy_]:=Which[yy<xx,Bl[yy,xx][t],True,Bl[xx,yy][t]]
vBu[xx_,yy_]:=Which[yy<xx,Bl[yy,xx][t]\[Conjugate],True,Bl[xx,yy][t]\[Conjugate]]


dotBm=Function[{a,b},
addListBm=SparseArray[{},{numbos,numbos}];
addListBl=SparseArray[{},{numbos,numbos}];
Do[addListBm[[x,a]]+= -I vBm[x,b],{x,a}];
Do[addListBm[[b,x]]+= I vBm[a,x],{x,b,numbos}];
Do[addListBl[[x,a]]+= -I vBl[b,x],{x,a-1}];
Do[addListBl[[a,x]]+= -I vBl[b,x],{x,a+1,numbos}];
{addListBm,addListBl}
];


dotBl=Function[{a,b},
addListBm=SparseArray[{},{numbos,numbos}];
addListBl=SparseArray[{},{numbos,numbos}];
Do[addListBm[[b,x]]+= I vBl[x,a],{x,b,numbos}];
Do[addListBm[[a,x]]+= I vBl[x,b],{x,a,numbos}];
{addListBm,addListBl}
];


dotBu=Function[{a,b},
addListBm=SparseArray[{},{numbos,numbos}];
addListBl=SparseArray[{},{numbos,numbos}];
Do[addListBm[[x,b]]+= -I vBu[x,a],{x,b}];
Do[addListBm[[x,a]]+= -I vBu[x,b],{x,a}];

Do[addListBl[[x,a]]+= -I vBm[b,x],{x,a-1}];
Do[addListBl[[b,x]]+= -I vBm[a,x],{x,b+1,numbos}];
Do[addListBl[[a,x]]+= -I vBm[b,x],{x,a+1,numbos}];
Do[addListBl[[x,b]]+= -I vBm[a,x],{x,b-1}];
{addListBm,addListBl}
];


hBm[a_,b_]:={dotBm[a,b],vBm[a,b]}
hBl[a_,b_]:={dotBl[a,b],vBl[a,b]}
hBu[a_,b_]:={dotBu[a,b],vBu[a,b]}
