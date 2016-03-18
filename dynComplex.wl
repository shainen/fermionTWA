(* ::Package:: *)

vEm[xx_,yy_]:=Which[yy<xx,Em[yy,xx][t]\[Conjugate],True,Em[xx,yy][t]]
vEl[xx_,yy_]:=Which[yy==xx,0,yy<xx,-El[yy,xx][t],True,El[xx,yy][t]]
vEu[xx_,yy_]:=Which[yy==xx,0,yy<xx,El[yy,xx][t]\[Conjugate],True,-El[xx,yy][t]\[Conjugate]]


(*tableEm[stuff_]:=Table[Table[stuff,{jj,ii,numferm}],{ii,numferm}]*)


(*tableEl[stuff_]:=Table[Table[stuff,{jj,ii+1,numferm}],{ii,numferm-1}]*)


(* ::Text:: *)
(*dotEm[a_, b_] := {vEm[x, a] -> -I vEm[x, b], vEm[b, x] -> I vEm[a, x], vEl[x, a] -> I vEl[b, x], vEl[a, x] -> -I vEl[b, x]}*)


(* ::Text:: *)
(*dotEl[a_, b_] := {vEm[b, x] -> -I vEl[x, a], vEm[a, x] -> I vEl[x, b]}*)


(* ::Text:: *)
(*dotEu[a_, b_] := {vEm[x, b] -> I vEu[x, a], vEm[x, a] -> -I vEu[x, b], vEl[x, a] -> I vEm[b, x], vEl[b, x] -> I vEm[a, x], vEl[a, x] -> -I vEm[b, x], vEl[x, b] -> -I vEm[a, x]}*)


(*dot[varType_,a_,b_]:=Switch[varType,Em,dotEm[a,b],El,dotEl[a,b],Eu,dotEu[a,b]]*)


dotEm=Function[{a,b},
addListEm=SparseArray[{},{numferm,numferm}];
addListEl=SparseArray[{},{numferm,numferm}];
Do[addListEm[[x,a]]+= -I vEm[x,b],{x,a}];
Do[addListEm[[b,x]]+= I vEm[a,x],{x,b,numferm}];
Do[addListEl[[x,a]]+= I vEl[b,x],{x,a-1}];
Do[addListEl[[a,x]]+= -I vEl[b,x],{x,a+1,numferm}];
{addListEm,addListEl}
];


dotEl=Function[{a,b},
addListEm=SparseArray[{},{numferm,numferm}];
addListEl=SparseArray[{},{numferm,numferm}];
Do[addListEm[[b,x]]+= -I vEl[x,a],{x,b,numferm}];
Do[addListEm[[a,x]]+= I vEl[x,b],{x,a,numferm}];
{addListEm,addListEl}
];


dotEu=Function[{a,b},
addListEm=SparseArray[{},{numferm,numferm}];
addListEl=SparseArray[{},{numferm,numferm}];
Do[addListEm[[x,b]]+= I vEu[x,a],{x,b}];
Do[addListEm[[x,a]]+= -I vEu[x,b],{x,a}];

Do[addListEl[[x,a]]+= I vEm[b,x],{x,a-1}];
Do[addListEl[[b,x]]+= I vEm[a,x],{x,b+1,numferm}];
Do[addListEl[[a,x]]+= -I vEm[b,x],{x,a+1,numferm}];
Do[addListEl[[x,b]]+= -I vEm[a,x],{x,b-1}];
{addListEm,addListEl}
];


bdot[bv_,ham_]:=bv'[t]==-I D[ham,bv[t]\[Conjugate]]


(*hamToFermEq=Function[{ham},
Total[Drop[#,-1]dot[Head[Last[#]],Last[#][[1]],Last[#][[2]]]&/@(List@@testHam)]
];*)


hEm[a_,b_]:={vEm[a,b],dotEm[a,b]}
hEl[a_,b_]:={vEl[a,b],dotEl[a,b]}
hEu[a_,b_]:={vEu[a,b],dotEu[a,b]}
