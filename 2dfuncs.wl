(* ::Package:: *)

(* ::Subsection:: *)
(*2d functions*)


cfneither[num_]:=IntegerDigits[num-1,length,3]


nfc[coord_]:=FromDigits[addl[coord],length]+1


addl[num_]:=Mod[num,length]


(* ::Subsubsection:: *)
(*fourier funcs*)


(*fermsbyfermstos1p1s2p2[data_?TensorQ]:=Transpose[Partition[data,{length,length}],{1,3,2}]*)


(*fou1d[data_,fou1_,fou2_]:=Transpose[Map[fou2,Transpose[Map[fou1,Transpose[data,{4,5,2,3,1}],{4}],{1,4,5,2,3}],{4}],{5,1,2,3,4}]*)


fermsbyfermstosxy1sxy2[data_?TensorQ]:=Transpose[Partition[Transpose[Partition[data,{length^2,length^2}],{3,4,1,2}],{length,length}],{2,5,3,6,1,4}]


fou2d[data_,fou1_,fou2_]:=Transpose[Map[fou1,Transpose[Map[fou2,data,{4}],{4,5,6,1,2,3}],{4}],{4,5,6,1,2,3}]


backtofermsbyferms[data_?TensorQ]:=Flatten/@Flatten[data,2]


makeMom[data_,fou1_,fou2_]:=backtofermsbyferms[fou2d[fermsbyfermstosxy1sxy2[data],fou1,fou2]]
