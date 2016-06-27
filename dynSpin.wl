(* ::Package:: *)

dotSp=Function[{a},
SparseArray[{{a,1}-> I 2 Sz[a][t],
{a,2}-> -I Sm[a][t]\[Conjugate]},{numbos,2}]
];


dotSm=Function[{a},
SparseArray[{{a,2}-> I Sm[a][t]},{numbos,2}]
];


dotSz=Function[{a},
SparseArray[{{a,1}-> - I Sm[a][t]},{numbos,2}]
];


hSpSp=Function[{a,b},
{dotSp[a]Sm[b][t]\[Conjugate]+Sm[a][t]\[Conjugate]dotSp[b],Sm[a][t]\[Conjugate]Sm[b][t]\[Conjugate]}
];


hSmSm=Function[{a,b},
{dotSm[a]Sm[b][t]+Sm[a][t]dotSm[b],Sm[a][t]Sm[b][t]}
];


hSpSm=Function[{a,b},
{dotSp[a]Sm[b][t]+Sm[a][t]\[Conjugate]dotSm[b],Sm[a][t]\[Conjugate]Sm[b][t]}
];


hSz=Function[{a},
{dotSz[a],Sz[a][t]}
];
