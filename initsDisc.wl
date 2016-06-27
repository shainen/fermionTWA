(* ::Package:: *)

(* ::Section:: *)
(*Fock state init*)


(* ::Subsection:: *)
(*biferm*)


there[x_]:=Length[Select[occupied,#==x&]]


discRandomEm[ii_,jj_]:=If[ii==jj,there[ii]-1/2,
If[there[ii]==there[jj],0,
(RandomChoice[{-1,1}]+I RandomChoice[{-1,1}])/2
]
]


discRandomEl[ii_,jj_]:=If[there[ii]!=there[jj],0,
(RandomChoice[{-1,1}]+I RandomChoice[{-1,1}])/2
]


(* ::Subsection:: *)
(*bibose*)


thereB[x_]:=Length[Select[occupiedB,#==x&]]


(*discRandomBm[ii_,jj_]:=If[ii==jj,thereB[ii]+1/2,
If[thereB[ii]==thereB[jj],If[thereB[ii]==0,0,(RandomChoice[{-2,2}]+I RandomChoice[{-2,2}])/2],
(RandomChoice[{-1,1}]+I RandomChoice[{-1,1}])/2
]
]*)


discRandomBm[ii_,jj_]:=If[ii==jj,thereB[ii]+1/2,
If[thereB[ii]==thereB[jj],0,
(RandomChoice[{-1,1}]+I RandomChoice[{-1,1}])/2
]
]


discRandomBl[ii_,jj_]:=If[thereB[ii]!=thereB[jj],0,
(RandomChoice[{-1,1}]+I RandomChoice[{-1,1}])/2
]


(* ::Subsection:: *)
(*single bose*)


discRandomSb[ii_]:=(RandomChoice[{-1,1}]+I RandomChoice[{-1,1}])/2


(* ::Subsection:: *)
(*spins*)


thereB[x_]:=Length[Select[occupiedB,#==x&]]


discRandomSm[ii_]:=(RandomChoice[{-1,1}]+I RandomChoice[{-1,1}])/2


discRandomSz[ii_]:=If[thereB[ii]==1,1/2,-1/2]
