(* ::Package:: *)

(* ::Subsubsection:: *)
(*Fock state init*)


there[x_]:=Length[Select[occupied,#==x&]]


thereB[x_]:=Length[Select[occupiedB,#==x&]]


random[mean_,var_]:=If[var==0,mean,RandomVariate[NormalDistribution[mean,Sqrt[var/2]]] + I RandomVariate[NormalDistribution[mean,Sqrt[var/2]]]]


randomReal[mean_,var_]:=If[var==0,mean,RandomVariate[NormalDistribution[mean,Sqrt[var]]]]


(*random[mean_,var_]:=mean*)


(*random[mean_,var_]:=mean*)


meanEm[ii_,jj_]:=KroneckerDelta[ii,jj](there[ii]-1/2)
meanEl[ii_,jj_]:=0


meanBm[ii_,jj_]:=KroneckerDelta[ii,jj](thereB[ii]+1/2)
meanBl[ii_,jj_]:=0


varEm[ii_,jj_]:=1/4-(1/2-there[ii])(1/2-there[jj])
varEl[ii_,jj_]:=1/4+(1/2-there[ii])(1/2-there[jj])


varBm[ii_,jj_]:=(-1/4+(1/2+thereB[ii])(1/2+thereB[jj]))(1-KroneckerDelta[ii,jj])
varBl[ii_,jj_]:=1/4+(1/2+thereB[ii])(1/2+thereB[jj])


randomEm[ii_,jj_]:=random[meanEm[ii,jj],varEm[ii,jj]]
randomEl[ii_,jj_]:=random[meanEl[ii,jj],varEl[ii,jj]]


randomEmWide[ii_,jj_]:=random[meanEm[ii,jj],2varEm[ii,jj]]
randomElWide[ii_,jj_]:=random[meanEl[ii,jj],2varEl[ii,jj]]


randomEmPlus[ii_,jj_]:=If[ii==jj,randomReal[meanEm[ii,jj],varEm[ii,jj]+1/4],random[meanEm[ii,jj],varEm[ii,jj]+1/4]]
randomElPlus[ii_,jj_]:=random[meanEl[ii,jj],varEl[ii,jj]+1/4]


(*randomBm[ii_,jj_]:=random[meanBm[ii,jj],varBm[ii,jj]]
randomBl[ii_,jj_]:=random[meanBl[ii,jj],varBl[ii,jj]]*)


randomBm[ii_,jj_]:=meanBm[ii,jj]
randomBl[ii_,jj_]:=meanBl[ii,jj]


randomB[mean_,var_]:=RandomVariate[NormalDistribution[mean,Sqrt[var/2]]] + I RandomVariate[NormalDistribution[0,Sqrt[var/2]]]


(*randomB[mean_,var_]:=mean*)


randomBoseInits:=Table[randomB[coh[[n]],1/2],{n,numbos}]
