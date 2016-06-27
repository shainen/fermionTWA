(* ::Package:: *)

(* ::Subsubsection:: *)
(*Fock state init*)


there[x_]:=Length[Select[occupied,#==x&]]


random[mean_,var_]:=If[var==0,mean,RandomVariate[NormalDistribution[mean,Sqrt[var/2]]]]


(*random[mean_,var_]:=mean*)


meanEm[ii_,jj_]:=KroneckerDelta[ii,jj](there[ii]-1/2)
meanEl[ii_,jj_]:=0


varEm[ii_,jj_]:=1/4-(1/2-there[ii])(1/2-there[jj])
varEl[ii_,jj_]:=1/4+(1/2-there[ii])(1/2-there[jj])


makeRandEm[ii_,jj_]:=random[meanEm[ii,jj],varEm[ii,jj]]
makeRandEl[ii_,jj_]:=random[meanEl[ii,jj],varEl[ii,jj]]


(*randMatEm := SymmetrizedArray[{i_, j_} :> makeRandEm[i, j], {numferm, numferm}, Symmetric[{1, 2}]] + I SymmetrizedArray[{i_, j_} :> makeRandEm[i, j], {numferm, numferm}, Antisymmetric[{1, 2}]]
randMatEl := Normal@SymmetrizedArray[{i_, j_} :> makeRandEl[i, j] + I makeRandEl[i, j], {numferm, numferm}, Antisymmetric[{1, 2}]]*)


randMatEm := SymmetrizedArray[{i_, j_} :> Re[discRandomEm[i, j]], {numferm, numferm}, Symmetric[{1, 2}]] + I SymmetrizedArray[{i_, j_} :>Im[discRandomEm[i, j]], {numferm, numferm}, Antisymmetric[{1, 2}]]
randMatEl := Normal@SymmetrizedArray[{i_, j_} :>discRandomEl[i,j], {numferm, numferm}, Antisymmetric[{1, 2}]]


(*randMomEm:=makeMom[randMatEm,InverseFourier,Fourier]
randMomEl:=makeMom[randMatEl,Fourier,Fourier]*)


randMomEm:=makeMom1d[randMatEm,InverseFourier,Fourier]
randMomEl:=makeMom1d[randMatEl,Fourier,Fourier]


initsEmPos:=(rm=randMatEm;Table[Table[Em[ii,jj][0]==rm[[ii,jj]],{jj,ii,numferm}],{ii,numferm}])
initsElPos:=(rm=randMatEl;Table[Table[El[ii,jj][0]==rm[[ii,jj]],{jj,ii+1,numferm}],{ii,numferm-1}])


initsEmMom:=(rm=randMomEm;Em[#1,#2][0]==rm[[#1,#2]]&@@@midPairs)
initsElMom:=(rm=randMomEl;El[#1,#2][0]==rm[[#1,#2]]&@@@lowPairs)


initsSmMom:=(
rm=Flatten[Fourier/@ArrayReshape[discRandomSm/@Range[numbos],{2,length}]];
{Sm[#][0]==rm[[#]]&/@Range[numbos],Sz[#][0]==Abs[rm[[#]]]^2-1/2&/@Range[numbos]})


randomB[mean_,var_]:=RandomVariate[NormalDistribution[mean,Sqrt[var/2]]] + I RandomVariate[NormalDistribution[0,Sqrt[var/2]]]


(*randomB[mean_,var_]:=mean*)


randomBoseInits:=Table[randomB[coh[[n]],1/2],{n,numbos}]


initsb:=MapThread[Equal,{Table[bh[ii][0],{ii,numbos}],randomBoseInits}]


initsPos:=Flatten[{initsb,initsEmPos,initsElPos}]


initsMom:=Flatten[{initsb,initsEmMom,initsElMom}]
