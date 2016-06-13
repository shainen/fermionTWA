(* ::Package:: *)

(* ::Subsubsection:: *)
(*Fock state init*)


there[x_]:=Length[Select[occupied,#==x&]]


discRandomEm[ii_,jj_]:=If[ii==jj,there[ii]-1/2,
If[there[ii]==there[jj],0,
(RandomChoice[{-1,1}]+I RandomChoice[{-1,1}])/2
]
]


discRandomEl[ii_,jj_]:=If[there[ii]!=there[jj],0,
(RandomChoice[{-1,1}]+I RandomChoice[{-1,1}])/2
]


thereB[x_]:=Length[Select[occupiedB,#==x&]]


discRandomBm[ii_,jj_]:=If[ii==jj,thereB[ii]+1/2,
If[thereB[ii]==thereB[jj],0,
(RandomChoice[{-1,1}]+I RandomChoice[{-1,1}])/2
]
]


discRandomBl[ii_,jj_]:=If[thereB[ii]!=thereB[jj],0,
(RandomChoice[{-1,1}]+I RandomChoice[{-1,1}])/2
]
