(* ::Package:: *)

(* ::Subsubsection:: *)
(*Fock state init*)


there[x_]:=Length[Select[occupied,#==x&]]


random[mean_,var_]:=If[var==0,mean,RandomVariate[NormalDistribution[mean,Sqrt[var/2]]] + I RandomVariate[NormalDistribution[mean,Sqrt[var/2]]]]


(*random[mean_,var_]:=mean*)


meanEm=Table[Table[KroneckerDelta[ii,jj](there[ii]-1/2),{jj,ii,numferm}],{ii,numferm}];
meanEl=Table[Table[0,{jj,ii+1,numferm}],{ii,numferm-1}];


varEm=Table[Table[1/4-(1/2-there[ii])(1/2-there[jj]),{jj,ii,numferm}],{ii,numferm}];
varEl=Table[Table[1/4+(1/2-there[ii])(1/2-there[jj]),{jj,ii+1,numferm}],{ii,numferm-1}];


randomEm:=Apply[random,(Transpose/@({meanEm,varEm}\[Transpose])),{2}]


randomEl:=Apply[random,(Transpose/@({meanEl,varEl}\[Transpose])),{2}]


randomB[mean_,var_]:=RandomVariate[NormalDistribution[mean,Sqrt[var/2]]] + I RandomVariate[NormalDistribution[0,Sqrt[var/2]]]


randomBoseInits:=Table[randomB[coh[[n]],1/2],{n,numbos}]
