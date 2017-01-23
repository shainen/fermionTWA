(* ::Package:: *)

(* ::Subsection:: *)
(*import*)


(*import[rname_,runs_,ignore_,comb_]:=(
list=Partition[Complement[Range[0,runs-1],ignore],comb];
eachOne={};
eachVarImb={};
Do[
tempAll=0;
tempVar=0;
Do[
Get["/data/shainen/"<>rname<>"/r"<>ToString[kk]<>"/spinchain.dat"];
data={TWASpDiscSU4,TWASpGauSU4,TWASpDelta};
AddTo[tempAll,data];
AddTo[tempVar,data[[All,3]]-data[[All,2]]^2];
,{kk,rr}];
AppendTo[eachOne,tempAll/comb];
AppendTo[eachVarImb,tempVar/comb];
,{rr,list}];
all=Total[eachOne]/Length[list];
avImb=all[[All,2]];
varImb=Total[eachVarImb]/Length[list];
)*)


import[rname_,runs_,ignore_,comb_]:=(
list=Partition[Complement[Range[1,runs],ignore],comb];
eachOne={};
Do[
tempAll=0;
Do[
Get["/projectnb/twambl/"<>rname<>"/r"<>ToString[kk]<>"/dataFermion.dat"];
data={fermOc,boseOc};
AddTo[tempAll,data];
,{kk,rr}];
AppendTo[eachOne,tempAll/comb];
,{rr,list}];
avg=Total[eachOne]/Length[list];
)


dir=StringSplit[ParentDirectory[],"/"][[4]];


import[dir,100,{},1];


Save["/projectnb/twambl/"<>dir<>"compiled.dat",{avg}];
