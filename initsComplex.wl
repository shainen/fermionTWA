(* ::Package:: *)

(* ::Subsubsection:: *)
(*Fock state init*)


there[x_]:=Length[Select[occupied,#==x&]]


thereB[x_]:=Length[Select[occupiedB,#==x&]]


random[mean_,var_]:=If[var==0,mean,RandomVariate[NormalDistribution[mean,Sqrt[var/2]]] + I RandomVariate[NormalDistribution[mean,Sqrt[var/2]]]]


(*random[mean_,var_]:=mean*)


meanEm[ii_,jj_]:=KroneckerDelta[ii,jj](there[ii]-1/2)
meanEl[ii_,jj_]:=0


meanBm[ii_,jj_]:=KroneckerDelta[ii,jj](thereB[ii]+1/2)
meanBl[ii_,jj_]:=0


varEm[ii_,jj_]:=1/4-(1/2-there[ii])(1/2-there[jj])
varEl[ii_,jj_]:=1/4+(1/2-there[ii])(1/2-there[jj])


varBm[ii_,jj_]:=-1/4+(1/2+thereB[ii])(1/2+thereB[jj])
varBl[ii_,jj_]:=1/4+(1/2+thereB[ii])(1/2+thereB[jj])


randomEm:=random[meanEm[#1,#2],varEm[#1,#2]]&@@@midPairs
randomEl:=random[meanEl[#1,#2],varEl[#1,#2]]&@@@lowPairs


randomBm:=random[meanBm[#1,#2],varBm[#1,#2]]&@@@midPairs
randomBl:=random[meanBl[#1,#2],varBl[#1,#2]]&@@@lowPairs


randomB[mean_,var_]:=RandomVariate[NormalDistribution[mean,Sqrt[var/2]]] + I RandomVariate[NormalDistribution[0,Sqrt[var/2]]]


randomBoseInits:=Table[randomB[coh[[n]],1/2],{n,numbos}]
