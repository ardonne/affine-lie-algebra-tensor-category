(* ::Package:: *)

(* ::Section:: *)
(*Begin Package*)


BeginPackage["qgmtc`"];


(* ::Subsection::Closed:: *)
(*Usage*)


initialize::usage = 
	"initialize[\"x\",r] initializes the type of affine Lie algebra, of type \"x\" and rank r, \
subject to the constraints displayed when the package was loaded.\n\
initialize[\"x\",r,level] initializes the type of affine Lie algebra, its rank and level.\n\
initialize[\"x\",r,level,rootfactor] initializes the type of affine Lie algebra, its rank, the level \
and the rootfactor, which sets the root of unity."
	
initializelevel::usage =
	"initializelevel[k] intializes the level to k."

setprecision::usage =
	"setprecision[prec] sets the precision to prec. Should be run before \
setrootofunity[rootfactor]. The default value is prec=100, which should be \
sufficient for most cases. If mathematica complains about ill-conditioned \
matrices, or if the fusion rules are inconsistent, etc., one can try to \
increase the precision."

initializerootofunity::usage =
	"initializerootofunity[rootfactor] sets the value of q to e^(2 \[Pi] i rootfactor/(tmax (k+g))), \
where k is the level, g the dual coxeter number of the affine Lie algebra, and \
tmax = 1 for the simply laced cases, tmax = 3 for g_2 and tmax = 2 for the other \
non-simply laced cases (g and tmax are set when intialize[\"x\",r] is run)."

setrootofunity::usage =
	"setrootofunity[rootfactor] sets the value of q to e^(2 \[Pi] i rootfactor/(tmax (k+g))), \
where k is the level, g the dual coxeter number of the affine Lie algebra, and \
tmax = 1 for the simply laced cases, tmax = 3 for g_2 and tmax = 2 for the other \
non-simply laced cases (g and tmax are set when intialize[\"x\",r] is run)."
	
possiblerootfactors::usage =
	"possiblerootfactors[\"x\",rank,level] gives the possible rootfactors \
for the selected type, rank and level."	
	
calculatefsymbols::usage = 
	"calculatefsymbols[] calculates the F-symbols, after the type of algebra, rank, \
level and root of unity have been selected. The calculation is done in several steps. Most \
importantly, the q-CG coefficients are constructed first. The pentagon equations are \
verified, as well as some properties of the F-matrices."
	
calculatersymbols::usage =
	"calculatersymbols[] calculates the R-symbols, after the F-symbols have been \
calculated. The hexagon equations are verified, as well as some properties of \
the R-matrices."

calculatemodulardata::usage =
	"calculatemodulardata[] calculates the modular data, once the F- and R-symbols are \
calculated: pivotal structure, Frodenius-Schur indicators, Frobenius-Perron \
dimensions, quantum dimensions, scaling dimensions, central charge adn the S-matrix."

diagonalizermatrices::usage = 
	"diagonalizermatrices[] diagonalizes the R-matrices, if they are not diagonal already. \
This will change both F- and R-symbols, so the pentagon and hexagon equations will be \
re-checked for security."

donotcheckpentagon::usage =
	"Run donotcheckpentagon[] (after the type of algebra, rank and level have been initialized) \
if you do not want the pentagon equations to be checked. \
Though it is of course safer to check the pentagon equations, the list of pentagon equations \
can be extremely long. By running donotcheckpentagon[], this list is not generated, and the \
pentagon equations are not checked. It is recommended to generate the R-symbols as well, so \
that at least the hexagon equations can be checked. If these hold, it is likely that the \
pentagon equations are also satisfied."

docheckpentagon::usage =
	"Run docheckpentagon[] if you ran donotcheckpentagon[] accidentally, \
but still want to check the pentagon equations."

displayinfo::usage = 
	"displayinfo[] displays some general information and basic instructions on how \
to use the package."

qCG::usage =
	"qcg[j1,{m1,n1},j2,{m2,n2},j3,{m3,n3},v1] gives the q-CG coefficient. \
The j's (highest weights) and m's (weights) are list of length 'rank'. \
The n's are integers distinguishing the weights (due to weight multiplicities) and \
v1 is an integer labeling which j3 in the tensor product of j1 and j2 one is considering \
(due to fusion multiplicity)."

Fsym::usage =
	"Fsym[a,b,c,d,e,f,{v1,v2,v3,v4}] gives the F-symbol. The a,b,...,f label the particles \
(lists of length 'rank'), while the v's (integers) label the four vertices."

Fmat::usage =
	"Fmat[a,b,c,d] gives the F-matrix. a,b,c,d label the particles \
(lists of length 'rank')."

Rsym::usage =
	"Rsym[a,b,c,{v1,v2}] gives the R-symbol. a,b,c label the particles \
(lists of length 'rank'), while the v's (integers) label the two vertices."

Rmat::usage =
	"Rmat[a,b,c,d] gives the R-matrix. a,b,c label the particles \
(lists of length 'rank')."

Nmat::usage =
	"Nmat[a] gives the fusion matrix for fusion with particle type a."

Nvertex::usage =
	"Nvertex[a,b,c] gives the number of vertices of type (a,b,c)."
	
Fusion::usage =
	"Fusion[a,b] gives the possible fusion outcomes of a x b, that is, without(!) \
taking fusion multiplicities into account. Use Nvertex[a,b,c] to obtain the number \
of vertices of type (a,b,c)."


(* ::Section:: *)
(*Begin `Private` Context*)


Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*General functions*)


displayinfo[] := With[{},
   textStyle = 
    Style[ #,  FontSize -> 15, FontFamily -> "Source Sans Pro" ] &;
   codeStyle = 
    Style[ #,  FontSize -> 14, FontFamily -> "Source Code Pro" ] &;
   
   Print[Sequence @@
     {
      Style[ "qgmtc package\n" , FontSize -> 36, 
       FontFamily -> "Source Sans Pro"],
      Style[ "Authors: Eddy Ardonne\n" , FontSize -> 15, 
       FontFamily -> "Source Sans Pro", Bold],
      Style[ "Many thanks to: Joost Slingerland, Gert Vercleyen\n" , 
       FontSize -> 15, FontFamily -> "Source Sans Pro", Bold],
      Style[ 
       "License: GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007\n\
" , FontSize -> 15, FontFamily -> "Source Sans Pro", Bold],
      Style[ 
       "Last revision: 2021-10-24\n\
" , FontSize -> 15, FontFamily -> "Source Sans Pro", Bold],


      textStyle["\nThis package is based on the paper:\n"],
      Style[ 
       "Clebsch-Gordan and 6j-coefficients for rank two quantum \
groups,\n" , FontSize -> 15, FontFamily -> "Source Sans Pro", Italic],
      textStyle["Eddy Ardonne, Joost Slingerland\n"],
      textStyle["J. Phys. A 43, 395205 (2010)\n"],
      textStyle["https://doi.org/10.1088/1751-8113/43/39/395205\n"],
      textStyle["https://arxiv.org/abs/1004.5456\n"],
      textStyle[
       "\n Quantum groups based on non-twisted affine Lie algebras \
give rise to modular tensor categories. With this package, one can, \
numerically, calculate the associated F- and R-symbols, as well as \
the modular data.\n"],
      textStyle[
       "The types of non-twisted affine Lie algebras possible are:\n"],
      TextGrid[{
        {"type", "rank", "tmax", "g", "note"},
        {"\"a\"", "r \[GreaterEqual] 1", "1", "r+1", "su(r+1)"},
        {"\"b\"", "r \[GreaterEqual] 3", "2", "2r-1", 
         "so(2r+1), so(5) is implemented as sp(4)"},
        {"\"c\"", "r \[GreaterEqual] 2", "2", "r+1", "sp(2r)"},
        {"\"d\"", "r \[GreaterEqual] 4", "1", "2r-2", "so(2r)"},
        {"\"e\"", "r \[Element] {6,7,8}", "1", "{12,18,30}"},
        {"\"f\"", "r == 4", "2", "9"},
        {"\"g\"", "r == 2", "3", "4"}
        }],
      textStyle[
       "\n One starts by selecting the type of affine Lie algebra and \
its rank by running "],
      codeStyle["initialize[\"x\",rank]"],
      textStyle[" where "],
      codeStyle["x \[Element] {a,b,c,d,e,f,g}"],
      textStyle[
       " and the rank satisfies the appropriate constraint form \
above. Subsequently, one selects the level, which has to be a \
positive integer, by running "],
      codeStyle["initializelevel[k]"],
      textStyle[
       ".\n Once the type of algebra, rank and level are set, the \
possible values of the deformation parameter q are fixed. q can take \
the values\n\
q = e^(2 \[Pi] i rootfactor/(tmax (k + g))) ,\n where k is the level, \
g the dual coxeter number of the \
algebra, and tmax is the ratio of the length of the long and short \
roots, if any (see the table above for the values of g and tmax). \
rootfactor should be relative prime with tmax(k+g). The value of q is \
set by running "],
      codeStyle["setrootofunity[rootfactor]"],
      textStyle[" .\n"],
      textStyle[
       "After the type of algebra, rank, level and rootfacter are \
set, one can proceed by calculating the F- and R-symbols, as well as \
the modular data, by running the commands (in this order)\n"],
      codeStyle["calculatefsymbols[]\n"],
      codeStyle["calculatersymbols[]\n"],
      codeStyle["calculatemodulardata[]\n"],
      textStyle[
       "If the R-matrices are not diagonal, they can be diagonalized \
using "],
      codeStyle["diagonalizermatrices[]"],
      textStyle[
       ".\nInformation on how to access the F- and R-symbols, can be \
found in the accompanying notebook, which contains some examples as \
well.\n"],
      textStyle["This information can be displayed by running "],
      codeStyle["displayinfo[]"],
      textStyle[".\n"],
      textStyle["\nSome technical notes.\n"],
      textStyle["If Mathematica complains about \
ill-conditioned matrices, or if the fusion rules are inconsistent, \
one can try to increase the precision (which is standard set to 100, \
so pretty high already) by running "],
      codeStyle["setprecision[precision]"],
      textStyle[" where precision should be an integer > 100.\n"],
      textStyle["When loading the package, the recusion limit is set to 10000.\n"],
      textStyle["The gauge choices made during the calculation follow, to a \
large extend, the ones described in the paper above."]
      }
    ];
   ];

Clear[nq];
nq[0,t_]:=0;
nq[1,t_]:=1;
nq[n_/;n<0,t_]:=-nq[-n,t];

Off[Solve::svars];

qnn[n_]:=(q^(n)-q^(-n))/(q^(1)-q^(-1));
qnnser[n_/;n==0]:=0;
qnnser[n_/;n>0]:=Sum[q^(j/2),{j,-(n-1),n-1,2}];
qnnser[n_/;n<0]:=Sum[-q^(j/2),{j,-(-n-1),-n-1,2}];
qnnser[n_/;n>0,t_]:=Sum[q^(j*t/(2)),{j,-(n-1),n-1,2}];
qnnser[n_/;n<0,t_]:=Sum[-q^(j*t/(2)),{j,-(-n-1),-n-1,2}];

nq[n_,t_]:=qnnser[n,t];

zeromatrix[dim_Integer]:= Table[0,{i1,1,dim},{i2,1,dim}];

precision=100;
typerankinitok=False;
typeranklevelinitok=False;
typeranklevelrootinitok=False;
fsymbolscalculated=False;
rsymbolscalculated=False;
modulardatacalculated=False;
pentagontobechecked=True;
recheck=False;
typerankinfo=True;
levelinfo=True;
rootfacinfo=True;


(* ::Subsection::Closed:: *)
(*Functions to access the data*)


qCG[j1_,m1x_,j2_,m2x_,j3_,m3x_,v_]:=qcg[j1,m1x,j2,m2x,j3,m3x,v];

Fsym[a_,b_,c_,d_,e_,f_,vvec_]:=fsym[a,b,c,d,e,f,vvec];

Fmat[a_,b_,c_,d_]:=fmat[a,b,c,d];

Rsym[a_,b_,c_,vvec_]:=rsym[a,b,c,vvec];

Rmat[a_,b_,c_]:=rmat[a,b,c];

Nmat[a_]:=nmat[a];

Nvertex[a_,b_,c_]:= nv[a,b,c];

Fusion[a_,b_]:= fusion[a,b];


(* ::Subsection::Closed:: *)
(*General initialization*)


$RecursionLimit=10000;

Clear[arangeok,ccor,acartanmatrix,cartanmatrix,athvec,asrootnorm,amarkvec,acomarkvec,dcoxeter,coxeter];

d[i_,j_]:=KroneckerDelta[i,j];

base[i_,j_,x_,y_]:=x d[i,j]+y  d[i,j+1]+y d[i,j-1];

arangeok[atype_,tw_,r_]:=If[r\[Element]Integers,If[tw==1,If[Or[atype==="a"&&r>=1,atype==="b"&&r>=3,atype==="c"&&r>=2,atype==="d"&&r>=4,atype==="e"&&MemberQ[{6,7,8},r],atype==="f"&&r==4,atype==="g"&&r==2],True,False],If[tw==3&&r==2&&atype==="g",True,If[tw==2,If[Or[atype==="a"&&r==1,atype==="b"&&r>=3,atype==="bb"&&r>=2,atype==="c"&&r>=2,atype==="f"&&r==4],True,False],False]]],False];

ccor[atype_,tw_,r_]:=ccor[atype,tw,r]=Flatten[DeleteCases[MapThread[If,{{atype==="a"&&tw==1,atype==="b"&&tw==1,atype==="c"&&tw==1,atype==="d"&&tw==1,atype==="e"&&tw==1,atype==="f"&&tw==1,atype==="g"&&tw==1,atype==="a"&&tw==2,atype==="b"&&tw==2,atype==="bb"&&tw==2,atype==="c"&&tw==2,atype==="f"&&tw==2,atype==="g"&&tw==3},{{{1,r+1,-1},{r+1,1,-1}},{{1,2,1},{2,1,1},{1,3,-1},{3,1,-1},{r,r+1,-1}},{{1,2,-1},{r+1,r,-1}},{{1,2,1},{2,1,1},{1,3,-1},{3,1,-1},{r,r+1,1},{r+1,r,1},{r-1,r+1,-1},{r+1,r-1,-1}},DeleteCases[{{r,r+1,1},{r+1,r,1},{r-2-Mod[r,2],r+1,-1},{r+1,r-2- Mod[r,2],-1},If[r==6,Sequence@@{{1,2,1},{2,1,1},{1,7,-1},{7,1,-1}}]},Null],{{3,4,-1}},{{2,3,-2}},{{2,1,-3}},{{1,2,1},{2,1,1},{1,3,-1},{3,1,-1},{r+1,r,-1}},{{2,1,-1},{r+1,r,-1}},{{2,1,-1},{r,r+1,-1}},{{4,3,-1}},{{3,2,-2}}}}],Null],1];

acartanmatrix[atype_,tw_,r_]:=acartanmatrix[atype,tw,r]=Table[base[i,j,2,-1]+Sum[ccor[atype,tw,r][[i1,3]]*d[i,ccor[atype,tw,r][[i1,1]]]*d[j,ccor[atype,tw,r][[i1,2]]],{i1,1,Length[ccor[atype,tw,r]]}],{i,1,r+1},{j,1,r+1}];

cartanmatrix[atype_,tw_,r_]:=cartanmatrix[atype,tw,r]=acartanmatrix[atype,tw,r][[2;;-1,2;;-1]];

athvec[atype_,tw_,r_]:=athvec[atype,tw,r]=Table[d[tw,1](d[i,1](d[atype,"a"]+2 d[atype,"c"]+d[atype,"e"](d[r,7]+d[r,8])+d[atype,"f"]+d[atype,"g"])+d[i,2](d[atype,"b"]+d[atype,"d"])+d[i,r](d[atype,"a"]+d[atype,"e"]*d[r,6]))+d[tw,2](d[i,1](2d[atype,"a"]+2 d[atype,"b"]+2d[atype,"bb"])+d[i,2]*d[atype,"c"]+d[i,r](d[atype,"f"]+d[atype,"c"]*d[r,2]))+d[tw,3]*d[i,r]*d[atype,"g"],{i,1,r}];

asrootnorm[atype_,tw_,r_]:=asrootnorm[atype,tw,r]=Table[1+d[tw,1]*d[atype,"c"]+d[tw,2](d[atype,"b"]+d[atype,"bb"])+d[i1,r](d[tw,1](d[atype,"b"]-d[atype,"c"]+d[atype,"f"]+2 d[atype,"g"])+d[tw,2](d[atype,"c"]-d[atype,"b"]-d[atype,"bb"]))+d[i1,r-1]*d[tw,1]*d[atype,"f"]+d[i1,2]*d[tw,2]*d[atype,"f"]+ d[i1,1](d[tw,2]*d[atype,"f"]+2d[tw,3]*d[atype,"g"]),{i1,1,r}];

amarkvec[atype_,tw_,r_]:=amarkvec[atype,tw,r]=Prepend[athvec[If[atype=!="bb",atype,"c"],1,r].Inverse[cartanmatrix[If[atype=!="bb",atype,"c"],1,r]]/If[tw>1,asrootnorm[If[atype=!="bb",atype,"a"],1,r],1],If[atype==="bb"||(atype==="a"&&tw==2),2,1]];

acomarkvec[atype_,tw_,r_]:=acomarkvec[atype,tw,r]=Prepend[((athvec[If[atype=!="bb",atype,"c"],1,r].Inverse[cartanmatrix[If[atype=!="bb",atype,"c"],1,r]]*If[tw>1,asrootnorm[If[atype=!="bb",atype,"a"],1,r],1])/asrootnorm[If[atype=!="bb",atype,"a"],1,r])If[atype==="bb"||(atype==="a"&&tw==2),Table[1+d[i,r],{i,1,r}],1],1];

dcoxeter[atype_,tw_,r_]:=dcoxeter[atype,tw,r]=Plus@@acomarkvec[atype,tw,r];

coxeter[atype_,tw_,r_]:=coxeter[atype,tw,r]=Plus@@amarkvec[atype,tw,r];


(* ::Subsection::Closed:: *)
(*Routines to initialization of the current algebra, rank, level and root of unity*)


initialize[atype_, rr_] :=
  Module[{},

   tw = 1;
   If[arangeok[atype, tw, rr],
    (* General initialization *)
    typerankinitok=False; (* Will be set to True once we're done *)
    typeranklevelinitok=False;
    typeranklevelrootinitok=False;
    fsymbolscalculated=False;
    rsymbolscalculated=False;
    modulardatacalculated=False;
    pentagontobechecked=True;
    recheck=False;
    Clear[raising, lowering, statespossible, statesneeded,
     tpstatesraising, basisraising, basislowering, stateraising,
     statelowering, tpstatevec, tpbasis, basis, basison, gramm, tpstates];
    Clear[fusion, nv, nmat, dualirreps, dualirrep, flist, fmatlist, pentlist, rlist, rmatlist,
    nvlist, maxmultiplicity, multiplicity, numoffusmultiplicities, irreps];
    Clear[qcg, fsym, fmat, fmatdim, fmatdimtab, rsym, rsyminv, rmat, 
  rmatinv, sign, phase, fsymold, rsymold, fmatold, rmatold, umat, 
  umatinv, numofirreps, numposroots, posroots, qdimvec, qd, qdtot2, 
  qdtot, qdimspositive, fpdimvec, irrepsdual, dual, selfdualvec, 
  selfdual, qdim1overfvec, pivotlist,  pivot, thetalist, theta, 
  hlist, hvalue, frobschurlist, frobschur, smat, cmat, tmat, modular, 
  pplus, pminus, modular2, centralcharge, modularrelationsok];
  Clear[Global`irreps, Global`flist, Global`fmatlist, Global`rlist, 
  Global`rmatlist, Global`maxmultiplicity,  Global`multiplicity, 
  Global`numberoffusionmultiplicities, Global`FPdimlist, 
  Global`qdimlist, Global`pivotlist, Global`thetalist, Global`hlist, 
  Global`FSlist, Global`smat, Global`tmat, Global`cmat, 
  Global`centralcharge, Global`modular, Global`unitary];
    type = atype;
    twist = tw;
    If[tw != 1, 
     Print["The code only works for the untwisted cases!"]];
    rank = rr;
    r = rr;
    c = 0;
    useweyl = False;
    If[r == 1, useweyl = False;];
    acartan = acartanmatrix[atype, tw, r];
    tw2abb = If[tw == 2 && (atype === "a" || atype === "bb"), True, False];
    tw2abbcor = If[tw2abb, 2, 1];
    exceptmult = 
     If[atype === "bb", r, 
      If[tw == 2 && atype === "b", r - 1, 
       If[tw == 2 && atype === "f", 2, 1]]];
    tvnorm = asrootnorm[atype, tw, r];
    tv = Table[
    Max[
      amarkvec[atype, tw, r][[i + 1]]/acomarkvec[atype, tw, r][[i + 1]],
      acomarkvec[atype, tw, r][[1]]
      ]
      , {i, 1, r}];
    tvc = 
     Table[
     Max[
       acomarkvec[atype, tw, r][[i + 1]]/amarkvec[atype, tw, r][[i + 1]], 
       amarkvec[atype, tw, r][[1]]
       ]
       , {i, 1, r}];
    th = athvec[atype, tw, r];
    comark = acomarkvec[atype, tw, r];
    g = dcoxeter[atype, tw, r];
    cartan = cartanmatrix[atype, tw, r];
    icartan = Inverse[cartan];
    qfm = Table[tw/tvnorm[[i]] Transpose[icartan][[i]], {i, 1, r}];
    scartan = 
     If[tw2abb, Table[tw/If[i == r, 2, 1] cartan[[i]], {i, 1, r}], 
      Table[tw/tvnorm[[i]] Transpose[cartan][[i]], {i, 1, r}]];
    tmax = Max[tv];
    tvec = tmax Table[1/i, {i, tv}];
    ipmat = 
     1/tmax Inverse[Table[cartan[[i]] 1/tvec[[i]], {i, 1, r}]];
    rho = Table[1, {j, 1, r}];
    a = icartan.rho;
    lenl = Max[Table[scartan[[i, i]], {i, 1, r}]];
    If[tw2abb && r == 1, lenl = 4];
    lens = Min[Table[scartan[[i, i]], {i, 1, r}]];
    dm = Table[{0, 0}, {i, 1, r}];
    Do[If[cartan[[i, j]] < 0, 
      Do[dm = Insert[dm, j, {i, -3}], {-cartan[[i, j]]}]
      ],
      {i, 1, r}, {j, 1, r}];
    
    (* Generate the roots of the algebra *)
    
    Clear[roots];
    pos = 1;
    roots = {th};
    While[pos <= Dimensions[roots][[1]], 
     For[
     i = 1, i <= r, i++, 
      If[roots[[pos, i]] > 0, 
       Do[If[Not[MemberQ[roots, roots[[pos]] - j cartan[[i]]]], 
         roots = Append[roots, roots[[pos]] - j cartan[[i]]]], {j, roots[[pos, i]]}]]
     ];
     pos = pos + 1
    ];
    roots = 
     Sort[roots, 
      Sum[(#1.icartan)[[j]] a[[j]], {j, 1, r}] >= Sum[(#2.icartan)[[j]] a[[j]], {j, 1, r}] &];
    na = Dimensions[roots][[1]];
    rootl = Table[roots[[i]].qfm.roots[[i]], {i, 1, na}];
    If[typerankinfo,
     Print["The type of algebra and rank have been set to ",{type,rank}];
    ];
    
    ccor[atype,tw,rr]=.;
    acartanmatrix[atype,tw,rr]=.;
    cartanmatrix[atype,tw,rr]=.;
    athvec[atype,tw,rr]=.;
    asrootnorm[atype,tw,rr]=.;
    amarkvec[atype,tw,rr]=.;
    acomarkvec[atype,tw,rr]=.;
    dcoxeter[atype,tw,rr]=.;

    typerankinitok=True;
    ,
    If[typerankinfo,
    Print["The type of algebra and the rank are not compatible!"];
    Print["Run initialize[\"x\",r] again to set the type of algebra and its rank."];
    ];
    ,
    If[typerankinfo,
    Print["The type of algebra and the rank are not compatible!"];
    Print["Run initialize[\"x\",r] again to set the type of algebra and its rank."];
    ];
    ];
   ];
   
   
initializelevel[lev_] :=
  Module[{n},  
  If[typerankinitok,
   If[IntegerQ[lev] && lev > 0,
     typeranklevelinitok=False; (* will be set to True once we're done *)
     typeranklevelrootinitok=False;
     fsymbolscalculated=False;
     rsymbolscalculated=False;
     modulardatacalculated=False;
     pentagontobechecked=True;
     recheck=False;
     Clear[raising, lowering, statespossible, statesneeded,
     tpstatesraising, basisraising, basislowering, stateraising,
     statelowering, tpstatevec, tpbasis, basis, basison, gramm, tpstates];
     Clear[fusion, nv, nmat, dualirreps, dualirrep, flist, fmatlist, pentlist, rlist, rmatlist,
     nvlist, maxmultiplicity, multiplicity, numoffusmultiplicities, irreps];
     Clear[qcg, fsym, fmat, fmatdim, fmatdimtab, rsym, rsyminv, rmat, 
  rmatinv, sign, phase, fsymold, rsymold, fmatold, rmatold, umat, 
  umatinv, numofirreps, numposroots, posroots, qdimvec, qd, qdtot2, 
  qdtot, qdimspositive, fpdimvec, irrepsdual, dual, selfdualvec, 
  selfdual, qdim1overfvec, pivotlist,  pivot, thetalist, theta, 
  hlist, hvalue, frobschurlist, frobschur, smat, cmat, tmat, modular, 
  pplus, pminus, modular2, centralcharge, modularrelationsok];
  Clear[Global`irreps, Global`flist, Global`fmatlist, Global`rlist, 
  Global`rmatlist, Global`maxmultiplicity,  Global`multiplicity, 
  Global`numberoffusionmultiplicities, Global`FPdimlist, 
  Global`qdimlist, Global`pivotlist, Global`thetalist, Global`hlist, 
  Global`FSlist, Global`smat, Global`tmat, Global`cmat, 
  Global`centralcharge, Global`modular, Global`unitary];
     level = lev;
     k = lev;
     rootofunity = 1/(g + k);
     Clear[rootfactor];
     rootfactors = Cases[Table[rf, {rf, 1, (tmax/rootofunity)}], x_ /; GCD[x, tmax/rootofunity] == 1];
     irreps = Flatten[
       Table[
        Table[m[j], {j, 1, rank}],
        Evaluate[
        Sequence@@Table[{m[i], 0, Floor[(level - Sum[m[i1]*comark[[i1 + 1]], {i1, 1, i - 1}])/comark[[i + 1]]]}, {i, 1, rank}]]
        ]
       , rank - 1];  
     airreps = 
      Table[Prepend[irreps[[i]], level - Plus @@ irreps[[i]]], {i, 1, Length[irreps]}];
     inprod[lp_, mm_, p_] := lp.qfm.mm + k p/tw2abbcor;
     
     (* Generate the states for all the weights in all the irreps at this level *)
     
     Clear[weights, wdim];
     
     Do[
      l = hw[[2 ;; -1]];
      fact[lp_, gr_] := 
       2/((l + rho).qfm.(l + rho) - (lp + rho).qfm.(lp + rho) + 2 gr (k + g)/tw2abbcor);
      Clear[w];
      w[0] = {hw};
      Do[w[i] = {}, {i, 1, c}];
      For[mm = 0, mm <= c, mm++, pos = 1;
       While[pos <= Dimensions[w[mm]][[1]], 
        For[i = 1, i <= r, i++, 
         If[w[mm][[pos, i + 1]] > 0, 
          Do[If[Not[
             MemberQ[w[mm], w[mm][[pos]] - j acartan[[i + 1]]]], 
            w[mm] = 
             Append[w[mm], w[mm][[pos]] - j acartan[[i + 1]]]], {j, w[mm][[pos, i + 1]]}]]]; pos = pos + 1];
       pos = 1;
       While[pos <= Dimensions[w[mm]][[1]], 
        If[w[mm][[pos, 1]] > 0, 
         Do[If[Not[MemberQ[w[mm + n], w[mm][[pos]] - n acartan[[1]]]],
            w[mm + n] = 
            Append[w[mm + n], w[mm][[pos]] - n acartan[[1]]]], {n, Min[w[mm][[pos, 1]], c - mm]}]];
        pos = pos + 1];
       pos = 1
       ];
      
      (* Strip off the zeroth label *)
      
      Do[
       Do[
        w[i] = Delete[w[i], {j, 1}]
        , {j, 1, Dimensions[w[i]][[1]]}]
        , {i, 0, c}];
      
      (* Sort the weigths according to their `heigth' *)
      
      Do[w[i] = 
        Sort[w[i], 
         Sum[#1[[j]]* a[[j]], {j, 1, r}] >= 
           Sum[#2[[j]] *a[[j]], {j, 1, r}] &], {i, 0, c}];
      
      weights[l] = w[0];
      
      
      (* Create the table of dimensions *)
      Clear[dims];
      Do[dims[i] = {}, {i, 0, c}];
      Do[
       Do[
        dims[i] = Insert[dims[i], {}, 1]
        , {j, 1, Dimensions[w[i]][[1]]}]
        , {i, 0, c}];
      dims[0] = Insert[dims[0], 1, {1, 1}];
      
      (* Generate the dimensions, up to grade c,
      which should be set to zero for our purposes *)
      
      Do[Do[If[Length[dims[i][[j]]] == 0, 
         dims[i] = 
          Insert[dims[i], 
           fact[w[i][[j]], 
             i] (Sum[
               Catch[For[n = tw2abbcor If[rootl[[a]] == lenl, tw, 1]; 
                  temp2 = 0, True, 
                  If[MemberQ[w[i - n], w[i][[j]] + roots[[a]]], 
                   temp2 += 
                    Catch[For[p1 = 1; temp1 = 0, True, 
                    If[MemberQ[w[i - n p1], 
                    w[i][[j]] + p1 roots[[a]]], 
                    temp1 += 
                    dims[i - n p1][[
                    Position[w[i - n p1], w[i][[j]] + p1 roots[[a]]][[
                    1, 1]], 
                    1]] (inprod[roots[[a]], w[i][[j]] + p1 roots[[a]],
                     n]); p1++, Throw[temp1]]]]; 
                   n += tw2abbcor If[rootl[[a]] == lenl, tw, 1], 
                   Throw[temp2]]]] + 
                Catch[For[n = tw2abbcor If[rootl[[a]] == lenl, tw, 1];
                   temp2 = 0, True, 
                  If[MemberQ[w[i - n], w[i][[j]] - roots[[a]]], 
                   temp2 += 
                    Catch[For[p2 = 1; temp1 = 0, True, 
                    If[MemberQ[w[i - n p2], 
                    w[i][[j]] - p2 roots[[a]]], 
                    temp1 += 
                    dims[i - n p2][[
                    Position[w[i - n p2], w[i][[j]] - p2 roots[[a]]][[
                    1, 1]], 
                    1]] (inprod[-roots[[a]], 
                    w[i][[j]] - p2 roots[[a]], n]); p2++, 
                    Throw[temp1]]]]; 
                   n += tw2abbcor If[rootl[[a]] == lenl, tw, 1], 
                   Throw[temp2]]]], {a, 1, (na - 1)/2}] + 
              Sum[Catch[
                For[temp = 0; p3 = 1, True, 
                 If[MemberQ[w[i], w[i][[j]] + p3 roots[[a]]], 
                  temp += 
                   dims[i][[
                    Position[w[i], w[i][[j]] + p3 roots[[a]]][[1, 1]],
                     1]] (inprod[roots[[a]], 
                    w[i][[j]] + p3 roots[[a]], 0]); p3++, 
                  Throw[temp]]]], {a, 1, (na - 1)/2}] + 
              Catch[For[n = tw2abbcor; temp2 = 0, True, 
                If[MemberQ[w[i - n], w[i][[j]]], 
                 temp2 += 
                  Catch[For[p4 = 1; temp1 = 0, True, 
                    If[MemberQ[w[i - n p4], w[i][[j]]], 
                    temp1 += 
                    If[Mod[p4, tw] == 0, r, 
                    exceptmult]  k p4 dims[i - n p4][[
                    Position[w[i - n p4], w[i][[j]]][[1, 1]], 1]]; p4++,
                     Throw[temp1]]]]; n += tw2abbcor, 
                 Throw[temp2]]]] + 
              If[tw2abb, 
               Sum[If[rootl[[a]] == lenl, 
                 Catch[For[n = 1; temp2 = 0, True, 
                    If[MemberQ[w[i - n], w[i][[j]] + 1/2 roots[[a]]], 
                    temp2 += 
                    Catch[For[p1 = 1; temp1 = 0, True, 
                    If[MemberQ[w[i - n p1], 
                    w[i][[j]] + p1 1/2  roots[[a]]], 
                    temp1 += 
                    dims[i - n p1][[
                    Position[w[i - n p1], 
                    w[i][[j]] + p1 1/2 roots[[a]]][[1, 1]], 
                    1]] (inprod[1/2 roots[[a]], 
                    w[i][[j]] + p1 1/2  roots[[a]], n]); p1++, 
                    Throw[temp1]]]]; n += 2, Throw[temp2]]]] + 
                  Catch[For[n = 1; temp2 = 0, True, 
                    If[MemberQ[w[i - n], w[i][[j]] - 1/2 roots[[a]]], 
                    temp2 += 
                    Catch[For[p1 = 1; temp1 = 0, True, 
                    
                    If[MemberQ[w[i - n p1], 
                    w[i][[j]] - p1 1/2  roots[[a]]], 
                    temp1 += 
                    dims[i - n p1][[
                    Position[w[i - n p1], 
                    w[i][[j]] - p1 1/2 roots[[a]]][[1, 1]], 
                    1]] (inprod[-1/2 roots[[a]], 
                    w[i][[j]] - p1 1/2  roots[[a]], n]); p1++, 
                    Throw[temp1]]]]; n += 2, Throw[temp2]]]], 0], {a, 
                 1, (na - 1)/2}], 0]), {j, 1}]; 
         If[useweyl, 
          Do[If[Length[
              dims[i][[
               Position[w[i], wms[[pos]].w[i][[j]]][[1, 1]]]]] == 0, 
            dims[i] = 
             Insert[dims[i], 
              dims[i][[j, 
               1]], {Position[w[i], wms[[pos]].w[i][[j]]][[1, 1]], 
               1}]], {pos, 2, awms}]]], {j, 1, 
         Dimensions[w[i]][[1]]}], {i, 0, c}];
      Do[dim[l, w[0][[i]]] = dims[0][[i, 1]], {i, 1, Length[dims[0]]}];
      
      
      Do[wdim[l, weights[l][[i]]] = dim[l, weights[l][[i]]], {i, 1, 
        Length[weights[l]]}];
      
      
      , {hw, airreps}];
     
     Do[
      raising[ir, w, alpha] = 
       If[MemberQ[weights[ir], w + cartan[[alpha]]], 
        w + cartan[[alpha]], {}];
      lowering[ir, w, alpha] = 
       If[MemberQ[weights[ir], w - cartan[[alpha]]], 
        w - cartan[[alpha]], {}];
      , {ir, irreps}, {w, weights[ir]}, {alpha, 1, r}];
     
     Clear[w, l];
     
     typeranklevelinitok=True;
     If[levelinfo,
     Print["The level has been set to ",level];
     Print["The possible roots of unity are ", 
      Exp[2 Pi I "rootfactor" rootofunity/(tmax)] // TraditionalForm, 
      ", with rootfactor an element of the set: ", rootfactors];
      ];
     ,
     If[levelinfo,
     Print["The level has to be a positive integer!"];
     Print["Run initializelevel[level] again to set the level."];
     ];
     ,
     If[levelinfo,
     Print["The level has to be a positive integer!"];
     Print["Run initializelevel[level] again to set the level."];
     ];
     ];
     ,
    Print["The type of algebra and rank are not correctly initialized, please do so first!"];
     ];
   ];   
   
initializerootofunity[rootfac_]:= setrootofunity[rootfac];   
   
setrootofunity[rootfac_] := Piecewise[{
    {
     If[MemberQ[rootfactors, rootfac],
       rootfactor = rootfac;
       q = N[Exp[2 Pi I rootfactor rootofunity/tmax], precision];
       If[rootfacinfo,
       Print["The rootfactor has been set to ",rootfactor];
       ];
       Print["The type of algebra, rank, level and rootfactor are initialized, and set to ",{type,rank,level,rootfactor}];
       Print["You can proceed to calculate the F-symbols :-)"];
       typeranklevelrootinitok = True;
       fsymbolscalculated=False;
       rsymbolscalculated=False;
       modulardatacalculated=False;
       recheck=False;
       Clear[statespossible, statesneeded,
       tpstatesraising, basisraising, basislowering, stateraising,
       statelowering, tpstatevec, tpbasis, basis, basison, gramm, tpstates];
       Clear[fusion, nv, nmat, dualirreps, dualirrep, flist, fmatlist, pentlist, rlist, rmatlist,
       nvlist, maxmultiplicity, multiplicity, numoffusmultiplicities];
       Clear[qcg, fsym, fmat, fmatdim, fmatdimtab, rsym, rsyminv, rmat, 
  rmatinv, sign, phase, fsymold, rsymold, fmatold, rmatold, umat, 
  umatinv, numofirreps, numposroots, posroots, qdimvec, qd, qdtot2, 
  qdtot, qdimspositive, fpdimvec, irrepsdual, dual, selfdualvec, 
  selfdual, qdim1overfvec, pivotlist,  pivot, thetalist, theta, 
  hlist, hvalue, frobschurlist, frobschur, smat, cmat, tmat, modular, 
  pplus, pminus, modular2, centralcharge, modularrelationsok];
  Clear[Global`irreps, Global`flist, Global`fmatlist, Global`rlist, 
  Global`rmatlist, Global`maxmultiplicity,  Global`multiplicity, 
  Global`numberoffusionmultiplicities, Global`FPdimlist, 
  Global`qdimlist, Global`pivotlist, Global`thetalist, Global`hlist, 
  Global`FSlist, Global`smat, Global`tmat, Global`cmat, 
  Global`centralcharge, Global`modular, Global`unitary];
       ,
       typeranklevelrootinitok = False;
       Print["The possible roots of unity are ", Exp[2 Pi I "rootfactor" rootofunity/(tmax)] // TraditionalForm, 
        ", with rootfactor an element from the set: ", rootfactors];
       Print["Run setrootofunity[rootfactor] again to select a valid rootfactor."];
       ,
       typeranklevelrootinitok = False;
       Print["The possible roots of unity are ", Exp[2 Pi I "rootfactor" rootofunity/(tmax)] // TraditionalForm, 
        ", with rootfactor an element from the set: ", rootfactors];
       Print["Run setrootofunity[rootfactor] again to select a valid rootfactor."];
       ];
     , typeranklevelinitok},
    {Print["The type of algebra and rank are not correctly initialized, please do so first!"];
     , Not[typerankinitok]},
    {Print["The level is not correctly initialized, please do so first!"];
     , typerankinitok && Not[typeranklevelinitok]}
    }];
    
    
possiblerootfactors[atype_, rank_, level_] :=
  Module[{tmax, g, rfpossible},
   If[arangeok[atype, 1, rank],
    If[IntegerQ[level] && level > 0,
     tmax = 
      Piecewise[{{1, 
         atype == "a" || atype == "d" || atype == "e"}, {2, 
         atype == "b" || atype == "c" || atype == "f"}, {3, 
         atype == "g"}}];
     g = Piecewise[{{rank + 1, atype == "a"}, {2*rank - 1, 
         atype == "b"}, {rank + 1, atype == "c"}, {2*rank - 2, 
         atype == "d"}, {12, atype == "e" && rank == 6}, {18, 
         atype == "e" && rank == 7}, {30, 
         atype == "e" && rank == 8}, {9, atype == "f"}, {4, 
         atype == "g"}}];
     rfpossible = 
      Cases[Table[i, {i, 1, tmax*(g + level)}], 
       x_ /; GCD[x, tmax*(g + level)] == 1];
     rfpossible
     , Print["The level is not a positve integer"];
     ]
    , Print["The type of algebra and rank are not compatible!"];
    ]
   ];    
   
   
initialize[atype_, rr_, level_] :=
    Module[{typerangeok, levelok},
   
   If[Not[arangeok[atype, tw, rr]],
    typerangeok = False;
    Print["The type of algebra and the rank are not compatible!"];,
    typerangeok = True;,
    typerangeok = False;
    Print["The type of algebra and the rank are not compatible!"];
    ];
   
   If[IntegerQ[level] && level > 0,
    levelok = True,
    Print["The level is not a positive integer!"];
    levelok = False;,
    Print["The level is not a positive integer!"];
    levelok = False;
    ];
   
   If[typerangeok && levelok,
    typerankinfo = False;
    levelinfo = False;
    initialize[atype, rr];
    initializelevel[level];
    Print["The type of algebra, rank, and level are initialized, and set to ",{atype,rr,level}];
    Print["The possible roots of unity are ", 
      Exp[2 Pi I "rootfactor" rootofunity/(tmax)] // TraditionalForm, 
      ", with rootfactor an element of the set: ", rootfactors];
    typerankinfo = True;
    levelinfo = True;
    ,
    Print["No initialization was done!"];
    ];
   
   ];   
   
initialize[atype_, rr_, level_, rootfac_] :=
  Module[{typerangeok, levelok, rootfacok, posrootfacs},
      If[Not[arangeok[atype, tw, rr]],
        typerangeok = False;
        Print["The type of algebra and the rank are not compatible!"];,
        typerangeok = True;,
        typerangeok = False;
        Print["The type of algebra and the rank are not compatible!"];
        ];
      If[IntegerQ[level] && level > 0,
        levelok = True,
        Print["The level is not a positive integer!"];
        levelok = False;,
        Print["The level is not a positive integer!"];
        levelok = False;
        ];
    If[typerangeok && levelok,
    posrootfacs = possiblerootfactors[atype, rr, level];
    If[MemberQ[posrootfacs, rootfac],
     rootfacok = True;,
     Print["The rootfactor should be a member of the set ", 
      posrootfacs];
     rootfacok = False;,
     Print["The rootfactor should be a member of the set ", 
      posrootfacs];
     rootfacok = False;
     ];
    ]; 
    If[typerangeok && levelok && rootfacok,
        typerankinfo = False;
        levelinfo = False;
        rootfacinfo = False;
        initialize[atype, rr];
        initializelevel[level];
        setrootofunity[rootfac];
        typerankinfo = True;
        levelinfo = True;
        rootfacinfo = True;
        ,
        Print["No initialization was done!"];
        ];
      ];   


setprecision[prec_] := With[{},
   If[IntegerQ[prec] && prec > 100,
     precision = prec;
     Print["The precision has been set to ", precision];
     ,
     precision = 100;
     Print["The precision should be an integer \[GreaterEqual] 100"];
     Print["The precision has been set to ", precision];
     ,
     precision = 100;
     Print["The precision should be an integer \[GreaterEqual] 100"];
     Print["The precision has been set to ", precision];
     ];
   ];         
   
donotcheckpentagon[] := With[{},
   pentagontobechecked = False;
   Print["You opted to hop over the step of checking the pentagon \
equations, so you proceed at your own risk. \n\
The (potentially very long) list of pentagon equations will not \
be generated, and the pentagon equations will not be checked. \n\
It is recommended that the R-symbols are also generated, so that \
the hexagon equations are checked. \
If they hold, it is likely the pentagon equations are also \
satisfied."];
   ];   
   
docheckpentagon[] := With[{},
   pentagontobechecked = True;
   Print["The pentagon equations will be checked."];
   ];   


(* ::Subsection::Closed:: *)
(*Routines for the inner product*)


cleargeneralinnerproduct[] :=
 With[{},
  Clear[generalinnerproduct];
  generalinnerproduct[hw_, operators_] := 
   generalinnerproduct[hw, operators] =
    Module[{leftmostlowerpos},
     If[
      
      (* Check if raising and lowering operators are consistent and only give states in the representation *)
      
      Sort[Cases[operators, x_ /; x[[2]] == 1][[All, 1]]] != 
        Sort[Cases[operators, x_ /; x[[2]] == -1][[All, 1]]] || Not[
        Module[{test, len},
         len = Length[operators];
         If[len > 0,
          test[len + 1] = hw;
          
          Do[test[pos] = 
            test[pos + 1] + (operators[[pos, 2]]) cartan[[operators[[pos, 1]]]]
            , {pos, len, 1, -1}];
          
          And @@ Table[
            MemberQ[weights[hw], test[pos]], {pos, 1, len}], True
          ]
         ]
        ],
      0,
      If[
       (* End of the recursion *)
       operators == {},
       1,
       If[
        (* (conjugate) raising operator acts on the highest weight *)
        operators[[1, 2]] == -1,
        0,
        (* Actual recursion step *)
        leftmostlowerpos = 
         Position[operators, x_ /; x[[2]] == -1, {1}, 1, 
           Heads -> False][[1, 1]];
        If[
         (* Check if lefmost lowering operator commutes with raising operator which is one position to the left *)
         operators[[leftmostlowerpos, 1]] != 
          operators[[leftmostlowerpos - 1, 1]],
         (* commutes, only one term contributes *)
         generalinnerproduct[hw, 
          ReplacePart[
           operators, {leftmostlowerpos -> 
             operators[[leftmostlowerpos - 1]], 
            leftmostlowerpos - 1 -> operators[[leftmostlowerpos]]}]]
            ,
         (* does not commute, two terms contribute *)
         generalinnerproduct[hw, 
           ReplacePart[
            operators, {leftmostlowerpos -> 
              operators[[leftmostlowerpos - 1]], 
             leftmostlowerpos - 1 -> operators[[leftmostlowerpos]]}]] +
          nq[(hw - Sum[cartan[[operators[[i, 1]]]], {i, 1, leftmostlowerpos - 2}])[[
             operators[[leftmostlowerpos, 1]]]], 
            tvec[[operators[[leftmostlowerpos, 1]]]]]*
           generalinnerproduct[hw, 
            Delete[operators, {{leftmostlowerpos}, {leftmostlowerpos - 1}}]]
        ]
        ]
       ]
      ]
     ]
  ];



clearinnerproduct[] :=
 With[{},
  Clear[innerproduct];
  innerproduct[hw1_, lowops1_, hw2_, lowops2_] := 
   innerproduct[hw1, lowops1, hw2, lowops2] = 
    If[hw1 != hw2 || Sort[lowops1] != Sort[lowops2], 0,
     Chop[
      generalinnerproduct[hw1, Flatten[{Table[{op1, 1}, {op1, Reverse[lowops1]}], Table[{op2, -1}, {op2, lowops2}]}, 1]]
      , 10^(-(Max[10, precision - 20]))]]
  ];

initializeinnerproduct[] :=
  With[{},
   clearinnerproduct[];
   cleargeneralinnerproduct[];
   ];


(* ::Subsection::Closed:: *)
(*Routines to construct the bases for the weight spaces*)


(*
generatestatespossible[] should not be necessary anymore, after the update dealing the precision in a better way.
It is only used when the standard way of generating the states needed does not work
*)

generatestatespossible[] :=
  
  Module[{weightsabove, allbasis, subs, subslength, notfound, subspos},
   
   Clear[statespossible];
   
   Do[
    
    statespossible[l, l] = {{}};
    
    Do[
     
     weightsabove = 
      DeleteCases[Table[{i, raising[l, currentweight, i]}, {i, 1, r}],
        x_ /; x[[2]] == {}];
     
     allbasis = Flatten[
       Table[
        Table[
         Prepend[j, weightsabove[[i, 1]]], {j, 
          statespossible[l, weightsabove[[i, 2]]]}]
        , {i, 1, Length[weightsabove]}]
       , 1];
     
     statespossible[l, currentweight] = allbasis;
     
     , {currentweight, weights[l][[2 ;; -1]]}];
    
    Clear[weightsabove, allbasis, subs, subslength, notfound, subspos];
    
    , {l, irreps}];
   
   ];
   
   
generatestatesneeded[] :=
  
  Module[{weightsabove, allbasis, subs, subslength, notfound, subspos},
   
   Clear[statesneeded];
   
   Do[
    
    statesneeded[l, l] = {{}};
    
    Do[
     weightsabove = 
      DeleteCases[Table[{i, raising[l, currentweight, i]}, {i, 1, r}],
        x_ /; x[[2]] == {}];
     
     allbasis = Flatten[
       Table[
        Table[
         Prepend[j, weightsabove[[i, 1]]]
         ,{j, statesneeded[l, weightsabove[[i, 2]]]}]
        , {i, 1, Length[weightsabove]}]
       , 1];
     
     subs = Subsets[Range[Length[allbasis]], {wdim[l, currentweight]}];
     subslength = Length[subs];
     notfound = True;
     subspos = 1;
     
     While[notfound && subspos <= subslength,
      If[
       MatrixRank[Table[
          Chop[innerproduct[l, w1, l, w2], 10^(-(Max[10, precision - 20]))]
          , {w1, allbasis[[subs[[subspos]]]]}
          , {w2, allbasis[[subs[[subspos]]]]}]
          ] == wdim[l, currentweight]
       ,
       notfound = False;
       statesneeded[l, currentweight] = allbasis[[subs[[subspos]]]];
       ];
      subspos++];
     
     If[notfound,
      
      Print["Reverting to the other method.", {l, currentweight}];
      
      If[statespossible[irreps[[1]], irreps[[1]]] == {{}}, Null, generatestatespossible[];];
      
      allbasis = statespossible[l, currentweight];
      
      subs = 
       Subsets[Range[Length[allbasis]], {wdim[l, currentweight]}];
      subslength = Length[subs];
      notfound = True;
      subspos = 1;
      
      While[notfound && subspos <= subslength,
       If[
        MatrixRank[Table[
           Chop[innerproduct[l, w1, l, w2], 
            10^(-(Max[10, precision - 20]))]
           , {w1, allbasis[[subs[[subspos]]]]}
           , {w2, allbasis[[subs[[subspos]]]]}]] == wdim[l, currentweight]
        ,
        notfound = False;
        statesneeded[l, currentweight] = allbasis[[subs[[subspos]]]]
        ];
       subspos++];
      
      If[notfound, 
       Print["No correct basis was found!! ", {l, currentweight}]];
      
      
      ];
     
     , {currentweight, weights[l][[2 ;; -1]]}];
    
    Clear[weightsabove, allbasis, subs, subslength, notfound, subspos];
    
    , {l, irreps}];
   
   ];   


constructgramm[] := With[{},
   Clear[gramm];
   Do[gramm[ir, w] = Table[
       Chop[innerproduct[ir, w1, ir, w2], 
        10^(-(Max[10, precision - 20]))]
       , {w1, statesneeded[ir, w]}
       , {w2, statesneeded[ir, w]}];
    , {ir, irreps}, {w, weights[ir]}];
   ];
   
constructbasis[] := Module[{norm},
 
     Clear[basis, basison];
   
   Do[
    basis[ir, w, i] =
     Table[If[j == i, 1, 0], {j, 1, wdim[ir, w]}] - 
      Sum[basis[ir, w, j] (basis[ir, w, j].gramm[ir, w])[[i]], {j, 1, 
        i - 1}];
        
    norm = basis[ir, w, i].gramm[ir, w].basis[ir, w, i];
    
    If[Chop[norm, 10^(-(Max[10, precision - 20]))] != 0, 
     basis[ir, w, i] = 
       Chop[1/Sqrt[norm] basis[ir, w, i], 
        10^(-(Max[10, precision - 20]))];, 
     Print["The norm of a state is zero! ", {ir, w, i}]];
    Clear[norm];
    , {ir, irreps}, {w, weights[ir]}, {i, 1, wdim[ir, w]}];
   
   Do[
    basison[ir, w] = Table[basis[ir, w, i], {i, wdim[ir, w]}]
    , {ir, irreps}, {w, weights[ir]}];
   
   ];   

 
checkweightspaceorthogonality[] := Module[{maxdev, tempdev},
   weightspaceorthogonal = True;
   maxdev = 0;
   Do[
    If[
      Chop[basison[ir, w].gramm[ir, w].Transpose[basison[ir, w]] - IdentityMatrix[wdim[ir, w]], 10^(-(Max[10, precision - 20]))] == 
       zeromatrix[wdim[ir, w]],
      Null
      ,
      Print[{ir, w}];
      weightspaceorthogonal = False;
      tempdev = 
       Max[(Abs /@ 
          Flatten[(basison[ir, w].gramm[ir, w].Transpose[
               basison[ir, w]] - IdentityMatrix[wdim[ir, w]])])];
      If[tempdev > maxdev, maxdev = tempdev];
      ];
    , {ir, irreps}, {w, weights[ir]}];

      If[weightspaceorthogonal,
    Print[
     "The constructed bases for the weightspaces are orthonormal :-)"],
    Print[
     "The constructed bases for the weightspaces are not orthonormal :-(, if the maximum deviation ", maxdev, 
     " is small, you can try to proceed at your own risk."]
    ];
   ]; 
   
initializeweightspacebasis[] := With[{},
   initializeinnerproduct[];
   generatestatesneeded[];
   constructgramm[];
   constructbasis[];
   checkweightspaceorthogonality[];
   ];     
         


(* ::Subsection::Closed:: *)
(*Routines to generate the fusion rules*)


myorthogonalize[vecs_] := Module[
  {numvecs, tempbas},
  numvecs = Length[vecs];
  Do[tempbas[i] =
    vecs[[i]] - 
     Sum[tempbas[
        j] (tempbas[j].vecs[[i]])/(tempbas[j].tempbas[j]), {j, 1, i - 1}]
   , {i, 1, numvecs}];
  Chop[Table[If[Chop[(tempbas[i].tempbas[i]), 10^(-(Max[10, precision - 20]))] != 0, 
     tempbas[i]/Sqrt[tempbas[i].tempbas[i]], tempbas[i]]
     , {i, 1, numvecs}], 10^(-(Max[10, precision - 20]))]
  ];
  
hwtpraising[hw1_, hw2_, hw3_, alpha_, vec_] := Module[
  {currstates, resvec, newstates, newweight, newstate, newstatepos, newcontrib},

  currstates = tpstates[hw1, hw2, hw3, hw3];
  resvec = Table[0, {i, 1, Length[tpstatesraising[hw1, hw2, hw3, alpha]]}];
  newstates = tpstatesraising[hw1, hw2, hw3, alpha];
  
  Do[
   newweight = raising[hw1, currstates[[i, 1, 1]], alpha];
   If[
    newweight != {},
    Do[
      newstate = {{newweight, j1}, currstates[[i, 2]]};
      newstatepos = Position[newstates, newstate][[1, 1]];
      newcontrib = 
       vec[[i]]* q^(currstates[[i, 2, 1, alpha]] * tvec[[alpha]]/(4))*
        basisraising[hw1, currstates[[i, 1, 1]], alpha][[currstates[[i, 1, 2]], j1]];
      resvec = 
       ReplacePart[resvec, 
        newstatepos -> resvec[[newstatepos]] + newcontrib];
      , {j1, 1, wdim[hw1, newweight]}];
    ];
   
   
   newweight = raising[hw2, currstates[[i, 2, 1]], alpha];
   If[
    newweight != {},
    Do[
      newstate = {currstates[[i, 1]], {newweight, j2}};
      newstatepos = Position[newstates, newstate][[1, 1]];
      newcontrib = 
       vec[[i]]* q^(-currstates[[i, 1, 1, alpha]] * tvec[[alpha]]/(4))*
        basisraising[hw2, currstates[[i, 2, 1]], alpha][[currstates[[i, 2, 2]], j2]];
      resvec = 
       ReplacePart[resvec, 
        newstatepos -> resvec[[newstatepos]] + newcontrib];
      , {j2, 1, wdim[hw2, newweight]}];
    ];
   
   
   , {i, 1, Length[vec]}];
  
  resvec
  
  ];  
  
  
hwtpraisingconditions[hw1_, hw2_, hw3_, alpha_] :=
  Chop[hwtpraising[hw1, hw2, hw3, alpha, Table[vars[i], {i, 1, Length[tpstates[hw1, hw2, hw3, hw3]]}]],10^(-(Max[10, precision - 20]))];  
  
  
hwtpraisingsol[hw1_, hw2_, hw3_] :=
 Chop[Solve[
   Table[(hwtpraisingconditions[hw1, hw2, hw3, alpha]) == 
     Table[0, {i, 1, Length[tpstatesraising[hw1, hw2, hw3, alpha]]}], {alpha, 1, r}],
   Table[vars[i], {i, 1, Length[tpstates[hw1, hw2, hw3, hw3]]}]], 10^(-(Max[10, precision - 20]))];  
   
generatefr[hw1_, hw2_, hw3_] := 
  Module[{tpdim, result, sol, variables, numofrawsol, rawstates, rawipmat, numofsol},
   tpdim = Length[tpstates[hw1, hw2, hw3, hw3]];
   
   (* If there are no possible tensor product states, hw3 will not be in
   the fusion product, so nothing needs to be done *)
   
   If[tpdim > 0,
    result = Table[vars[i], {i, 1, tpdim}];
    sol = hwtpraisingsol[hw1, hw2, hw3];
    If[Length[sol] != 1, 
     Print["There is a problem with the heighest weight solution."]];
    sol = sol[[1]];
    
    (* For all the varialbles, the state is a hw, so nv[]=tpdim *)
   
     If[sol == {},
     fusion[hw1, hw2] = Append[fusion[hw1, hw2], hw3];
     nv[hw1, hw2, hw3] = tpdim;
     ];
    
    (* In this case, 
    we need to find the number of independent solutions *)
    
    If[sol != {} && tpdim > Length[sol],
     variables = Union[Flatten[Variables /@ sol[[All, 2]]]];
     numofrawsol = Length[variables];
     result = result /. sol;
     
     rawstates = Table[
       result /. 
        Table[variables[[j]] -> If[j == i, 1, 0], {j, 1, numofrawsol}]
       , {i, 1, numofrawsol}];
     
     
     rawipmat = 
      Table[rawstates[[i]].rawstates[[j]], {i, 1, numofrawsol}, {j, 1, numofrawsol}];
         
     numofsol = MatrixRank[rawipmat];
     If[numofsol > 0,
      fusion[hw1, hw2] = Append[fusion[hw1, hw2], hw3];
      nv[hw1, hw2, hw3] = numofsol;
      ];
     ];
    ];
   ];   
   
   
generateraisingloweringoperators[] := With[{},
   
   Do[
    If[raising[ir, w, alpha] == {}
     , stateraising[ir, w, alpha] = {},
     stateraising[ir, w, alpha] =
      Table[
       generalinnerproduct[ir, 
        Flatten[{Table[{op1, 1}, {op1, Append[Reverse[statesneeded[ir, raising[ir, w, alpha]][[j]]], alpha]}],
           Table[{op2, -1}, {op2, statesneeded[ir, w][[i]]}]}, 1]]
       , {i, wdim[ir, w]}, {j, wdim[ir, raising[ir, w, alpha]]}]
     ]
    , {ir, irreps}, {w, weights[ir]}, {alpha, 1, rank}];
   
   Do[
    If[lowering[ir, w, alpha] == {}
     , statelowering[ir, w, alpha] = {},
     statelowering[ir, w, alpha] =
      Table[
       generalinnerproduct[ir, 
        Flatten[{Table[{op1, 1}, {op1, Reverse[statesneeded[ir, lowering[ir, w, alpha]][[j]]]}], 
          Table[{op2, -1}, {op2, Prepend[statesneeded[ir, w][[i]], alpha]}]}, 1]]
       , {i, wdim[ir, w]}, {j, wdim[ir, lowering[ir, w, alpha]]}]
     ]
    , {ir, irreps}, {w, weights[ir]}, {alpha, 1, rank}];
   
   Do[
    If[raising[ir, w, alpha] == {},
     basisraising[ir, w, alpha] = {},
     basisraising[ir, w, alpha] = basison[ir, w].stateraising[ir, w, alpha].Transpose[basison[ir, raising[ir, w, alpha]]]
     ]
    , {ir, irreps}, {w, weights[ir]}, {alpha, 1, rank}];
   
   Do[
    If[lowering[ir, w, alpha] == {},
     basislowering[ir, w, alpha] = {},
     basislowering[ir, w, alpha] = basison[ir, w].statelowering[ir, w, alpha].Transpose[basison[ir, lowering[ir, w, alpha]]]
     ]
    , {ir, irreps}, {w, weights[ir]}, {alpha, 1, rank}];
   
   ];   
   
initializetpstates[] := With[{},
   
   Do[
    Module[{res},
      res = 
       Cases[Table[{w1, (w - w1)}, {w1, weights[hw1]}], x_ /; MemberQ[weights[hw2], x[[2]]]];
      tpstates[hw1, hw2, hw3, w] =
       Flatten[
        Table[{{res[[i, 1]], v1}, {res[[i, 2]], v2}}
        , {i, 1, Length[res]}, {v1, 1, wdim[hw1, res[[i, 1]]]}, {v2, 1, wdim[hw2, res[[i, 2]]]}], 2];
      ];
    , {hw1, irreps}, {hw2, irreps}, {hw3, irreps}, {w, weights[hw3]}];
   
   Do[
    Module[{res},
      res = 
       Cases[Table[{w1, (hw3 + cartan[[alpha]] - w1)}, {w1, weights[hw1]}], x_ /; MemberQ[weights[hw2], x[[2]]]];
      tpstatesraising[hw1, hw2, hw3, alpha] =
       Flatten[
        Table[{{res[[i, 1]], v1}, {res[[i, 2]], v2}}
        , {i, 1, Length[res]}, {v1, 1, wdim[hw1, res[[i, 1]]]}, {v2, 1, wdim[hw2, res[[i, 2]]]}], 2];
      ];
    , {hw1, irreps}, {hw2, irreps}, {hw3, irreps}, {alpha, 1, rank}];
   
   ];   
   
   
intializefusionrules[] := With[{},
  Clear[fusion];
  Clear[nv];
  Clear[nmat];
  Do[fusion[hw1, hw2] = {};, {hw1, irreps}, {hw2, irreps}];
  ];
  
     
checkfusionrules[] := Module[{dualpos, tempok, irreppos},
  
  fusionrulesok = True;
  
  Do[
   nmat[a] = Table[
     If[Head[nv[a, b, c]] === Integer, nv[a, b, c], 0]
     , {b, irreps}, {c, irreps}]
  , {a, irreps}];
  
  tempok = True;
  Do[
   dualpos = 
    Flatten[Position[Table[fusion[a, b], {b, irreps}], x_ /; MemberQ[x, irreps[[1]]]]];
   If[
    Length[dualpos] == 1, Null, fusionrulesok = False; 
    tempok = False;];
   , {a, irreps}];
   
  If[tempok, Null, 
   Print["Not every particle type has a unique dual :-("];];
  
  dualpos = 
   Table[Position[Table[fusion[a, b], {b, irreps}], x_ /; MemberQ[x, irreps[[1]]]][[1, 1]], {a, irreps}];
  dualirreps = irreps[[dualpos]];
  Do[
  dualirrep[irreps[[i]]] = irreps[[dualpos[[i]]]]
  , {i, 1, Length[irreps]}];
  
  tempok = True;
  Do[
   If[nmat[a].nmat[b] == nmat[b].nmat[a], Null,
     fusionrulesok = False;
     tempok = False;
     ];
   , {a, irreps}, {b, irreps}];
  If[tempok, Null, 
   Print["The fusion rules are not associatve :-("];];
  
  
  tempok = True;
  Do[irreppos[irreps[[i]]] = i, {i, 1, Length[irreps]}];
  
  Do[
   If[
     nmat[a][[irreppos[b], irreppos[c]]] == 
      nmat[b][[irreppos[a], irreppos[c]]] == 
      nmat[b][[irreppos[dualirrep[c]], irreppos[dualirrep[a]]]] == 
      nmat[dualirrep[a]][[irreppos[dualirrep[b]], 
       irreppos[dualirrep[c]]]]
     , Null, fusionrulesok = False;
     tempok = False;
     ];
   , {a, irreps}, {b, irreps}, {c, irreps}];
  
  If[tempok, Null, 
   Print["The vertex properties of the fusionrules are not satisfied :-("];];
  
  
  If[fusionrulesok,
   Print["The calculated fusion rules are consistent :-)"];,
   Print["The calculated fusion rules are not consistent :-( something went wrong..."];
   ];
  
  ];
  
generatefusionrules[] :=
  Module[{},
   
   generateraisingloweringoperators[];
   
   initializetpstates[];
   
   intializefusionrules[];
   
   Do[generatefr[hw1, hw2, hw3], {hw1, irreps}, {hw2, irreps}, {hw3, irreps}];
   
   checkfusionrules[];
   
   flist = 
    Flatten[Table[{a, b, c, d, e, f, {v1, v2, v3, v4}}
    , {a, irreps}, {b, irreps}, {c, irreps}, {e, fusion[a, b]}, {d, fusion[e, c]}
    , {f, Cases[fusion[b, c], x_ /; MemberQ[fusion[a, x], d]]}
    , {v1, nv[a, b, e]}, {v2, nv[e, c, d]}, {v3, nv[b, c, f]}, {v4, nv[a, f, d]}]
    , 9];
   
   fmatlist = flist[[All, 1 ;; 4]] // Union;
   
   If[pentagontobechecked,
   pentlist = 
    Flatten[Table[{a, b, c, d, e, f, g, fp, gp, {v1, v2, v3, v4, v5, v6}}
       , {a, irreps}, {b, irreps}, {c, irreps}, {d, irreps}, {f, fusion[a, b]}, {g, fusion[f, c]}, {e, fusion[g, d]}
       , {gp, Cases[fusion[c, d], x_ /; MemberQ[fusion[f, x], e]]}
       , {fp, Cases[fusion[b, gp], x_ /; MemberQ[fusion[a, x], e]]}
       , {v1, nv[a, b, f]}, {v2, nv[f, c, g]}, {v3, nv[g, d, e]}, {v4, nv[c, d, gp]}, {v5, nv[b, gp, fp]}, {v6, nv[a, fp, e]}]
       , 14];
    ];
       
       
   
   rlist = Flatten[
     Table[{a, b, c, {v1, v2}}
     , {a, irreps}, {b, irreps}, {c, fusion[a, b]}, {v1, nv[a, b, c]}, {v2, nv[a, b, c]}]
     , 4];
   
   rmatlist = rlist[[All, 1 ;; 3]] // Union;
   
   nvlist = Table[
     nv[Sequence @@ rmatlist[[i]]]
     , {i, 1, Length[rmatlist]}];
   
   maxmultiplicity = Max[nvlist];
   
   multiplicity = Not[(nvlist // Union) == {1}];
   
   numoffusmultiplicities = Count[nvlist, x_ /; x > 1];
   
   Global`irreps = irreps;
   
   Global`flist = flist;
   Global`fmatlist = fmatlist;
   Global`rlist = rlist;
   Global`rmatlist = rmatlist;
   Global`maxmultiplicity = maxmultiplicity;
   Global`multiplicity = multiplicity;
   Global`numberoffusionmultiplicities = numoffusmultiplicities;
         
   ]; 


(* ::Subsection::Closed:: *)
(*Routines to generate the tensor product highest weight states*)


sethwstates[hw1_, hw2_, hw3_] := Module[
   {tpdim, result, sol, variables, numofstates, rawstates, 
    numofrawsol, rawipmat, subs, subslength, notfound, subspos},
   
   tpdim = Length[tpstates[hw1, hw2, hw3, hw3]];
   numofstates = nv[hw1, hw2, hw3];
   
   (* Although it should not be possible, let's check if tpdim > 0 *)

      If[tpdim == 0,
    Print["No states in the tensor product, something's wrong!"];,
    
    result = Table[vars[i], {i, 1, tpdim}];
    sol = hwtpraisingsol[hw1, hw2, hw3];
    If[Length[sol] != 1, 
     Print["There is a problem with the heighest weight solution."]];
    sol = sol[[1]];
    
    (* sol={}, so for each veriable there is a hwstate *)
    
    If[sol == {},
     result = IdentityMatrix[tpdim];
     ];
    
    (* In this case, we need to construct nv independent states *)
   
     If[sol != {} && tpdim > Length[sol],
     variables = Union[Flatten[Variables /@ sol[[All, 2]]]];
     numofrawsol = Length[variables];
     result = result /. sol;
     rawstates = Table[
       result /. Table[variables[[j]] -> If[j == i, 1, 0], {j, 1, numofrawsol}]
       , {i, 1, numofrawsol}];
     rawipmat = 
      Table[rawstates[[i]].rawstates[[j]]
      , {i, 1, numofrawsol}, {j, 1, numofrawsol}];
     
     subs = Subsets[Range[numofrawsol], {numofstates}];
     subslength = Length[subs];
     notfound = True;
     subspos = 1;
     While[notfound && subspos <= subslength,
      If[
       MatrixRank[rawipmat[[subs[[subspos]], subs[[subspos]]]]] == numofstates,
       notfound = False;
       result = Chop[myorthogonalize[rawstates[[subs[[subspos]]]]], 10^(-(Max[10, precision - 20])) ];
       ];
      subspos++];
     If[notfound, 
      Print["The number of hw states (as previously determined) were not found, so there's a problem"]];
     ];
    
    If[result == Table[vars[i], {i, 1, tpdim}],
     Print["No hw states were found, somthing's wrong!"]
     ];
    
    Do[
     tpstatevec[hw1, hw2, hw3, i, {}] = result[[i]];
     , {i, 1, numofstates}];
    
    ];
   ];
   
generatehwstates[] := Module[{},
  Clear[tpstatevec];
  Do[sethwstates[hw1, hw2, hw3], {hw1, irreps}, {hw2, irreps}, {hw3, fusion[hw1, hw2]}];
  ];   


(* ::Subsection::Closed:: *)
(*Routines for the tensor product lowering operations & calculation of the q - CG coefficients*)


tplowering[hw1_, hw2_, hw3_, w3_, alpha_, vec_] := Module[
   {currstates, resvec, newstates, newweight, newstate, newstatepos, 
    newcontrib},
   currstates = tpstates[hw1, hw2, hw3, w3];
   newstates = tpstates[hw1, hw2, hw3, w3 - cartan[[alpha]]];
   resvec = Table[0, {i, 1, Length[newstates]}];
   
   Do[
    newweight = lowering[hw1, currstates[[i, 1, 1]], alpha];
    If[
     newweight != {},
     Do[
       newstate = {{newweight, j1}, currstates[[i, 2]]};
       newstatepos = Position[newstates, newstate][[1, 1]];
       newcontrib = 
        vec[[i]]*q^(currstates[[i, 2, 1, alpha]]* tvec[[alpha]]/(4))*
         basislowering[hw1, currstates[[i, 1, 1]], alpha][[currstates[[i, 1, 2]], j1]];
       resvec = 
        ReplacePart[resvec, newstatepos -> resvec[[newstatepos]] + newcontrib];
       , {j1, 1, wdim[hw1, newweight]}];
     ];
    
    
    newweight = lowering[hw2, currstates[[i, 2, 1]], alpha];
    If[
     newweight != {},
     Do[
       newstate = {currstates[[i, 1]], {newweight, j2}};
       newstatepos = Position[newstates, newstate][[1, 1]];
       newcontrib = 
        vec[[i]]* q^(-currstates[[i, 1, 1, alpha]]*tvec[[alpha]]/(4))*
         basislowering[hw2, currstates[[i, 2, 1]], alpha][[currstates[[i, 2, 2]], j2]];
       resvec = 
        ReplacePart[resvec, newstatepos -> resvec[[newstatepos]] + newcontrib];
       , {j2, 1, wdim[hw2, newweight]}];
     ];
    
    
    , {i, 1, Length[vec]}];
   
   resvec
   
   ];
   
lowertpstates[] := With[{},
   
   Do[
     tpstatevec[hw1, hw2, hw3, v, state] = 
       Chop[tplowering[hw1, hw2, hw3, w3 + cartan[[state[[1]]]], 
         state[[1]], tpstatevec[hw1, hw2, hw3, v, state[[2 ;; -1]]]],10^(-(Max[10, precision - 20]))];
     , {hw1, irreps}, {hw2, irreps}, {hw3, fusion[hw1, hw2]}
     , {w3, weights[hw3][[2 ;; -1]]}, {state, statesneeded[hw3, w3]}, {v, 1, nv[hw1, hw2, hw3]}];
   
   ];   
   
generatetpbasis[] := With[{},
   
   Do[
     tpbasis[hw1, hw2, hw3, v, {w3, i}] = 
       Chop[Sum[basison[hw3, w3][[i, j]] *tpstatevec[hw1, hw2, hw3, v, statesneeded[hw3, w3][[j]]], {j, 1, wdim[hw3, w3]}] , 10^(-(Max[10, precision - 20]))];
     , {hw1, irreps}, {hw2, irreps}, {hw3, fusion[hw1, hw2]}
     , {w3, weights[hw3]}, {i, 1, wdim[hw3, w3]}, {v, 1, nv[hw1, hw2, hw3]}];
   
   ]; 
   
checkorthonormality[] := Module[{maxdev, tempdev},
   orthonormalityok = True;
   maxdev = 0;
   
   Do[
    If[(Chop[ Table[(tpbasis[hw1, hw2, hw3, v, {w3, i}].tpbasis[hw1, hw2, 
                hw3, v, {w3, j}]), {i, 1, wdim[hw3, w3]}, {j, 1, 
              wdim[hw3, w3]}] - IdentityMatrix[wdim[hw3, w3]] , 10^(-(Max[10, precision - 20]))] // Flatten // Union) == {0},
      Null,
      
      orthonormalityok = False;
      
      tempdev = 
       Max[Abs /@ (Flatten[
           Table[(tpbasis[hw1, hw2, hw3, v, {w3, i}].tpbasis[hw1, hw2, hw3, v, {w3, j}])
           , {i, 1, wdim[hw3, w3]}, {j, 1, wdim[hw3, w3]}] - IdentityMatrix[wdim[hw3, w3]]])];
      
      If[tempdev > maxdev, maxdev = tempdev];
      
      ];
      
    , {hw1, irreps}, {hw2, irreps}, {hw3, fusion[hw1, hw2]}
    , {w3, weights[hw3]}, {v, 1, nv[hw1, hw2, hw3]}];
   
   If[orthonormalityok,
    Print["The calculated q-CG coefficients satisfy the orthonormality conditions :-)"],
    Print["The constructed q-CG coefficients do not satisfy the orthonormalilty conditions :-( If the maximum deviation ", maxdev, 
      " is small, you can try to proceed at your own risk; typically, the numerical error in the F-sybmols is smaller. Increasing the precision, might also help"];
    ];
   ]; 
   
   
generatealltpstates[] := With[{},
   
   lowertpstates[];
   
   generatetpbasis[];
   
   checkorthonormality[];
   
   ];
   
storeqcgcoeffs[] := With[{},
   
   numqCGcoef = 0;
   
   Do[
    With[
     {currstates = tpstates[hw1, hw2, hw3, w3]},
     Do[
       qcg[hw1, currstates[[j, 1]], hw2, currstates[[j, 2]], hw3, {w3, i}, v] = 
        Chop[tpbasis[hw1, hw2, hw3, v, {w3, i}][[j]] , 10^(-(Max[10, precision - 20]))];
       numqCGcoef++;
       , {i, 1, wdim[hw3, w3]}, {j, 1, Length[currstates]}];
     ]
    , {hw1, irreps}, {hw2, irreps}, {hw3, fusion[hw1, hw2]}
    , {w3, weights[hw3]}, {v, 1, nv[hw1, hw2, hw3]}];
   
   ];
   
generateqcgcoefficients[] := With[{},
   
   generatealltpstates[];
   
   storeqcgcoeffs[];
   
   ];               
            


(* ::Subsection::Closed:: *)
(*Routines to calculate the F - symbols*)


constructfsymbols[] := With[{},
   
   Clear[fsym, fmat, fmatdim];
   
   Do[fsym[Sequence @@ flist[[i]]] = Chop[Sum[
        qcg[flist[[i, 1]], {m1, d1}, flist[[i, 2]], {m2, d2}, flist[[i, 5]], {m12, d12}, flist[[i, 7, 1]]] *
          qcg[flist[[i, 5]], {m12, d12}, flist[[i, 3]], {m3, d3}, flist[[i, 4]], {flist[[i, 4]], 1}, flist[[i, 7, 2]]] *
          qcg[flist[[i, 2]], {m2, d2}, flist[[i, 3]], {m3, d3}, flist[[i, 6]], {m23, d23}, flist[[i, 7, 3]]] *
          qcg[flist[[i, 1]], {m1, d1}, flist[[i, 6]], {m23, d23}, flist[[i, 4]], {flist[[i, 4]], 1}, flist[[i, 7, 4]]],
        {m1, weights[flist[[i, 1]]]}, {d1, 1, wdim[flist[[i, 1]], m1]},
        {m2, weights[flist[[i, 2]]]}, {d2, 1, wdim[flist[[i, 2]], m2]},
        {m12, Cases[weights[flist[[i, 5]]], x_ /; (m1 + m2) == x]}, {d12, 1, wdim[flist[[i, 5]], m12]},
        {m3, Cases[weights[flist[[i, 3]]], x_ /; (m12 + x) == flist[[i, 4]]]}, {d3, 1, wdim[flist[[i, 3]], m3]}, 
        {m23, Cases[weights[flist[[i, 6]]], x_ /; (m2 + m3) == x]}, {d23, 1, wdim[flist[[i, 6]], m23]}
        ] , 10^(-(Max[10, precision - 20]))];
    , {i, 1, Length[flist]}];
   
   Do[
    fmat[Sequence @@ fm] =
     Flatten[
      Table[
       fsym[fm[[1]], fm[[2]], fm[[3]], fm[[4]], e, f, {v1, v2, v3, v4}],
       {e, Cases[fusion[fm[[1]], fm[[2]]], x_ /; MemberQ[fusion[x, fm[[3]]], fm[[4]]]]},
       {f, Cases[fusion[fm[[2]], fm[[3]]], x_ /; MemberQ[fusion[fm[[1]], x], fm[[4]]]]},
       {v1, nv[fm[[1]], fm[[2]], e]},
       {v2, nv[e, fm[[3]], fm[[4]]]},
       {v3, nv[fm[[2]], fm[[3]], f]},
       {v4, nv[fm[[1]], f, fm[[4]]]}
       ]
      , {{1, 3, 4}, {2, 5, 6}}];
    
    fmatdim[Sequence @@ fm] = Length[fmat[Sequence @@ fm]];
    , {fm, fmatlist}];
   
   fmatdimtab = Table[fmatdim[Sequence @@ fm], {fm, fmatlist}];
   
   ];
   

checkpentagon[] := Module[{maxdev, tempdev},
   
   pentholds = True;
   pentundecidable = False;
   maxdev = 0;
   
   If[pentagontobechecked,
   If[Not[recheck],Print["Checking the ", Length[pentlist]," pentagon equations..."];];
   If[recheck,Print["Re-checking the ", Length[pentlist]," pentagon equations..."];];
   Do[If[Chop[
        Sum[(fsym[i[[6]], i[[3]], i[[4]], i[[5]], i[[7]], i[[9]], {i[[10, 2]], i[[10, 3]], i[[10, 4]], v1}] *
        fsym[i[[1]], i[[2]], i[[9]], i[[5]], i[[6]], i[[8]], {i[[10, 1]], v1, i[[10, 5]], i[[10, 6]]}])
        , {v1, nv[i[[6]], i[[9]], i[[5]]]}] - 
         Sum[(fsym[i[[1]], i[[2]], i[[3]], i[[7]], i[[6]], h, {i[[10, 1]], i[[10, 2]], v2, v3}]*
         fsym[i[[1]], h, i[[4]], i[[5]], i[[7]], i[[8]], {v3, i[[10, 3]], v4, i[[10, 6]]}]*
             fsym[i[[2]], i[[3]], i[[4]], i[[8]], h, i[[9]], {v2, v4, i[[10, 4]], i[[10, 5]]}])
             , {h, Quiet[Cases[fusion[i[[2]], i[[3]]], x_ /; MemberQ[fusion[i[[1]], x], i[[7]]]
                  && MemberQ[fusion[x, i[[4]]], i[[8]]]]]}
             , {v2, nv[i[[2]], i[[3]], h]}, {v3, nv[i[[1]], h, i[[7]]]}, {v4, nv[h, i[[4]], i[[8]]]}], 10^(-(Max[10, precision - 20]))] == 0,
      Null,
      
      pentholds = False;
      tempdev = 
       Abs[
        Sum[(fsym[i[[6]], i[[3]], i[[4]], i[[5]], i[[7]], i[[9]], {i[[10, 2]], i[[10, 3]], i[[10, 4]], v1}]*
             fsym[i[[1]], i[[2]], i[[9]], i[[5]], i[[6]], i[[8]], {i[[10, 1]], v1, i[[10, 5]], i[[10, 6]]}])
             , {v1, nv[i[[6]], i[[9]], i[[5]]]}] - 
         Sum[(fsym[i[[1]], i[[2]], i[[3]], i[[7]], i[[6]], 
             h, {i[[10, 1]], i[[10, 2]], v2, v3}] *
             fsym[i[[1]], h, i[[4]], i[[5]], i[[7]], i[[8]], {v3, i[[10, 3]], v4, i[[10, 6]]}]*
              fsym[i[[2]], i[[3]], i[[4]], i[[8]], h, i[[9]], {v2, v4, i[[10, 4]], i[[10, 5]]}])
              , {h, Quiet[Cases[fusion[i[[2]], i[[3]]], x_ /; MemberQ[fusion[i[[1]], x], i[[7]]] && 
               MemberQ[fusion[x, i[[4]]], i[[8]]]]]}
               , {v2, nv[i[[2]], i[[3]], h]}, {v3, nv[i[[1]], h, i[[7]]]}, {v4, nv[h, i[[4]], i[[8]]]}]];
      
      If[tempdev > maxdev, maxdev = tempdev];
      ,
      If[Not[pentundecidable], 
        Print["At least one of the pentagon equations is not decidable! Something went wrong :-("];
        pentundecidable = True;];
      ];
    
    
    , {i, pentlist}];
    ,
     If[Not[recheck],
     Print["The pentagon equations were not checked, because you opted not \
to do so. Proceed with care!"];
     ];
     If[recheck,
     Print["The pentagon equations were not (re)-checked, because you opted not \
to do so. Proceed with care!"];
    ];

  ];
   
   If[pentagontobechecked,
  If[pentholds && Not[pentundecidable],
    Print["The pentagon equations are satisfied :-)"];,
    Print["The pentagon equations are not satisfied :-(, the maximum deviation is: ", maxdev, " something went wrong..."];];
  ];

   If[Not[recheck],
   If[multiplicity,
    If[numoffusmultiplicities == 1,
     Print["There is one fusion multiplicity."];, 
     Print["There are ", numoffusmultiplicities," fusion multiplicities."]];
    Print["The largest fusion multiplicity is: ", maxmultiplicity];
    ,
    Print["There are no fusion multiplicities."];
    ];
    ];
    
   
   fsymsreal = 
    And @@ (Element[#, Reals] & /@ 
       Table[Chop[ fsym[Sequence @@ i] , 10^(-20) ], {i, flist}]);
   
   If[
    And @@ Table[
       (Chop[ (fmat[Sequence @@ fm].Conjugate[Transpose[fmat[Sequence @@ fm]]] - 
              IdentityMatrix[fmatdim[Sequence @@ fm]]) , 10^(-20) ] // Flatten // Union) == {0}
       , {fm, fmatlist}] && And @@ Table[(Chop[ (Conjugate[Transpose[fmat[Sequence @@ fm]]].fmat[Sequence @@ fm] - 
              IdentityMatrix[fmatdim[Sequence @@ fm]]) , 10^(-20) ] // Flatten // Union) == {0}
       , {fm, fmatlist}],
    fmatunitary = True;
    Print["The F-matrices are unitary."];,
    fmatunitary = False;
    Print[
     "The F-matrices are not all unitary. This can happen if q is not a primitive root of unity."];
    ];
   
   
   If[
    And @@ Table[
       ( Chop[ (fmat[Sequence @@ fm].Transpose[fmat[Sequence @@ fm]] - IdentityMatrix[fmatdim[Sequence @@ fm]]) , 10^(-20) ] // Flatten // Union) == {0}
       , {fm, fmatlist}] &&
    And @@ Table[( Chop[ (Transpose[fmat[Sequence @@ fm]].fmat[Sequence @@ fm] - IdentityMatrix[fmatdim[Sequence @@ fm]]) , 10^(-20)] // Flatten // Union) == {0}
       , {fm, fmatlist}],
    fsymsFTFone = True;
    ,
    fsymsFTFone = False;
    ];
   
   If[fsymsreal && fsymsFTFone,
    Print["The F-matrices are orthogonal."]
    ];
   
   If[fsymsreal && Not[fsymsFTFone],
    Print["The F-matrices are real."];
    Print["The F-matrices do not all satisfy F.(F^T) = (F^T).F = 1. One should check if this is reasonalbe or not!"];
    ];
   
   If[Not[fsymsreal] && fsymsFTFone,
    Print["The F-matrices are not all real."];
    Print["The F-matrices satisfy F.(F^T) = (F^T).F = 1"];
    ];
   
   If[Not[fsymsreal] && Not[fsymsFTFone],
    Print["The F-matrices are not all real."];
    Print["The F-matrices do not all satisfy F.(F^T) = (F^T).F = 1. One should check if this is reasonalbe or not!"];
    ];
    
    If[(pentagontobechecked && pentholds && Not[pentundecidable]) || Not[pentagontobechecked],
     fsymbolscalculated = True;
     If[Not[recheck],
      Print["You can proceed to calculate the R-symbols :-)"];
     ];
    ];
   
   ];      
   
   
   
calculatefsymbols[] :=
 With[{},
  
  If[typeranklevelrootinitok,
  
  Print["Constructing the bases for the weightspaces..."];
  initializeweightspacebasis[];
  
  Print["Constructing the fusion rules..."];
  generatefusionrules[];
  
  Print["Constructing the bases for the tensorproduct weightspaces..."];
  generatehwstates[];
  generateqcgcoefficients[];
  
  Print["Calculating the ", Length[flist], " F-symbols from the ", 
   numqCGcoef, " q-CG coefficients..."];
   
  constructfsymbols[];
   
  checkpentagon[]; 
  
  Print["Done :-)"];
  
  ,
  Print["The type of algebra, rank, level and/or rootfactor were not (correctly) initialized, please do so first!"];
  ,
  Print["The type of algebra, rank, level and/or rootfactor were not (correctly) initialized, please do so first!"];
  ];

  
  ];   


(* ::Subsection::Closed:: *)
(*Routines to calculate the R - symbols*)


constructrsymbols[] := Module[
   {hw1, hw2, hw3, currtpstates, curr1stweights, goodpositions, 
    temphex, goodposonesign, signeqleft, goodsignvars, 
    goodsignvarsunion, goodsignfirstpos, solsign, goodposonephase, 
    phaseeqleft, goodphasevars, goodphasevarsunion, goodphasefirstpos,
     solphase, firstlinpos, lineqleft},
   
   Clear[rsym, rmat, sign, phase];
   
   Do[
    hw1 = rlist[[i, 1]];
    hw2 = rlist[[i, 2]];
    hw3 = rlist[[i, 3]];
    
    If[
     
     nv[hw1, hw2, hw3] == 1,
     
     currtpstates = tpstates[hw1, hw2, hw3, hw3];
     curr1stweights = currtpstates[[All, 1, 1]];
     
     goodpositions = Position[
        Table[
         Or @@ Table[MemberQ[curr1stweights, curr1stweights[[i]] - cartan[[j]]], {j, 1, rank}]
         , {i, 1, Length[curr1stweights]}]
        , False] // Flatten;
     
     
     If[(curr1stweights[[goodpositions]] // Union // Length) != 1, 
      Print["There is more than one different minimal weight. The \
last one is being used, and no check is done if the other(s) give the \
same result or not... If the hexagon equations are not satisfied, \
this issue should be investigated further!"]];
     
     goodpositions = 
      Position[curr1stweights, curr1stweights[[goodpositions[[-1]]]]] // Flatten;
     
     If[Length[goodpositions] == 1,
      rsym[rlist[[i, 1]], rlist[[i, 2]], rlist[[i, 3]], rlist[[i, 4]]] = 
        q^(tmax/2 currtpstates[[goodpositions[[1]], 1, 1]].qfm.currtpstates[[goodpositions[[1]], 2, 1]])*
         qcg[hw1, currtpstates[[goodpositions[[1]], 1]], hw2, currtpstates[[goodpositions[[1]], 2]], hw3, {hw3, 1}, rlist[[i, 4, 1]]]/
          qcg[hw2, currtpstates[[goodpositions[[1]], 2]], hw1, currtpstates[[goodpositions[[1]], 1]], hw3, {hw3, 1}, rlist[[i, 4, 1]]];
      ,
      rsym[rlist[[i, 1]], rlist[[i, 2]], rlist[[i, 3]], rlist[[i, 4]]] =
        sign[rlist[[i, 1]], rlist[[i, 2]], rlist[[i, 3]], rlist[[i, 4]]]*
         q^(tmax/2 currtpstates[[goodpositions[[1]], 1, 1]].qfm.currtpstates[[goodpositions[[1]], 2, 1]])*
         Sqrt[
           Sum[(qcg[hw1, currtpstates[[pos, 1]], hw2, currtpstates[[pos, 2]], hw3, {hw3, 1}, rlist[[i, 4, 1]]])^2, {pos, goodpositions}]]/
          Sqrt[Sum[(qcg[hw2, currtpstates[[pos, 2]], hw1, currtpstates[[pos, 1]], hw3, {hw3, 1}, rlist[[i, 4, 1]]])^2, {pos, goodpositions}]];
      ];
     
     ,
     
     Do[
       rsym[hw1, hw2, hw3, {v1, v2}] = phase[hw1, hw2, hw3, {v1, v2}];
       
       , {v1, 1, nv[hw1, hw2, hw3]}, {v2, 1, nv[hw1, hw2, hw3]}];
     
     ];
    
    , {i, 1, Length[rlist]}];
   
   
   temphex =
    DeleteCases[
     Table[
      
      Chop[(Sum[rsym[i[[1]], i[[2]], i[[5]], {i[[7, 1]], v8}]*
           fsym[i[[1]], i[[2]], i[[3]], i[[4]], i[[5]], i[[6]], {v8, i[[7, 2]], v9, i[[7, 4]]}]*
           rsym[i[[3]], i[[2]], i[[6]], {v9, i[[7, 3]]}]
           ,{v8, nv[i[[1]], i[[2]], i[[5]]]}, {v9, nv[i[[3]], i[[2]], i[[6]]]}]
         -
         Sum[fsym[i[[2]], i[[1]], i[[3]], i[[4]], i[[5]], j, {i[[7, 1]], i[[7, 2]], v5, v6}]*
           rsym[j, i[[2]], i[[4]], {v6, v7}]*
           fsym[i[[1]], i[[3]], i[[2]], i[[4]], j, i[[6]], {v5, v7, i[[7, 3]], i[[7, 4]]}]
          , {j, Cases[irreps, x_ /; MemberQ[fusion[i[[1]], i[[3]]], x] &&
              MemberQ[fusion[i[[2]], x], i[[4]]]]}
              , {v5, nv[i[[1]], i[[3]], j]}, {v6, nv[i[[2]], j, i[[4]]]}, {v7,  nv[i[[2]], j, i[[4]]]}]) , 10^(-20) ]
      , {i, flist}]
     , 0];
      
   goodposonesign = Position[temphex, x_Complex + c1_Complex sign[y1___]] // Flatten;
   
   If[Length[goodposonesign] > 0, signeqleft = True];
   
   While[
    signeqleft
    ,
    signeqleft = False;
    goodsignvars = Variables /@ temphex[[goodposonesign]] // Flatten;
    goodsignvarsunion = Union[goodsignvars];
    goodsignfirstpos = Table[Position[goodsignvars, currvar, {1}, 1][[1, 1]], {currvar, goodsignvarsunion}];
    solsign = 
     Solve[Table[
       temphex[[goodposonesign[[i]]]] == 0, {i, 1, Length[goodposonesign]}]];
    
    If[Length[solsign] != 1, Print["There is more than one solution for the signs! Something went wrong!"]];
    solsign = Chop[ solsign[[1]] , 10^(-(Max[10, precision - 20])) ];
    
    Do[
    rsym[Sequence @@ rlist[[i]]] = Chop[ rsym[Sequence @@ rlist[[i]]] /. solsign , 10^(-(Max[10, precision - 20]))];
    , {i, 1, Length[rlist]}];
    
    temphex =
     DeleteCases[
      Table[
       
       Chop[(Sum[rsym[i[[1]], i[[2]], i[[5]], {i[[7, 1]], v8}]*
            fsym[i[[1]], i[[2]], i[[3]], i[[4]], i[[5]], i[[6]], {v8, i[[7, 2]], v9, i[[7, 4]]}]*
            rsym[i[[3]], i[[2]], i[[6]], {v9, i[[7, 3]]}]
            , {v8, nv[i[[1]], i[[2]], i[[5]]]}, {v9, 
            nv[i[[3]], i[[2]], i[[6]]]}]
          -
          Sum[fsym[i[[2]], i[[1]], i[[3]], i[[4]], i[[5]], j, {i[[7, 1]], i[[7, 2]], v5, v6}]*
            rsym[j, i[[2]], i[[4]], {v6, v7}]*
            fsym[i[[1]], i[[3]], i[[2]], i[[4]], j, i[[6]], {v5, v7, i[[7, 3]], i[[7, 4]]}]
           , {j, Cases[irreps,  x_ /; MemberQ[fusion[i[[1]], i[[3]]], x] &&
             MemberQ[fusion[i[[2]], x], i[[4]]]]}
           , {v5, nv[i[[1]], i[[3]], j]}, {v6, nv[i[[2]], j, i[[4]]]}, {v7, nv[i[[2]], j, i[[4]]]}]) , 10^(-20) ]
       
       , {i, flist}]
      
      , 0];
    
    goodposonesign = Position[temphex, x_Complex + c1_Complex sign[y1___]] // Flatten;
    
    If[Length[goodposonesign] > 0, signeqleft = True];
    
    ];
   
   
   
   If[
    MemberQ[Table[rsym[Sequence @@ rs], {rs, rlist}], sign[___]],
    Print["There are variables of type sign[___] left, but no linear equations with just one sign[___] variable."];
    ];
   
   goodposonephase = 
    Position[temphex, x_Complex + c1_Complex phase[y1___]] // Flatten;
   
   If[Length[goodposonephase] > 0, phaseeqleft = True];
   
   While[
    phaseeqleft
    ,
    phaseeqleft = False;
    goodphasevars = Variables /@ temphex[[goodposonephase]] // Flatten;
    goodphasevarsunion = Union[goodphasevars];
    goodphasefirstpos = Table[Position[goodphasevars, currvar, {1}, 1][[1, 1]], {currvar, goodphasevarsunion}];
    
    solphase = 
     Solve[Table[
       temphex[[goodposonephase[[i]]]] == 0, {i, 1, Length[goodposonephase]}]];
    
    If[Length[solphase] != 1, 
     Print["There is more than one solution for the phases! Something went wrong!"]];
    solphase = Chop[ solphase[[1]] , 10^(-(Max[10, precision - 20])) ];
    
    Do[rsym[Sequence @@ rlist[[i]]] = Chop[ rsym[Sequence @@ rlist[[i]]] /. solphase , 10^(-(Max[10, precision - 20])) ];
    , {i, 1, Length[rlist]}];
    
    temphex =
     DeleteCases[
      Table[
       Chop[(Sum[rsym[i[[1]], i[[2]], i[[5]], {i[[7, 1]], v8}]*
            fsym[i[[1]], i[[2]], i[[3]], i[[4]], i[[5]], i[[6]], {v8, i[[7, 2]], v9, i[[7, 4]]}]*
            rsym[i[[3]], i[[2]], i[[6]], {v9, i[[7, 3]]}]
            , {v8, nv[i[[1]], i[[2]], i[[5]]]}, {v9, nv[i[[3]], i[[2]], i[[6]]]}]
          -
          Sum[fsym[i[[2]], i[[1]], i[[3]], i[[4]], i[[5]], j, {i[[7, 1]], i[[7, 2]], v5, v6}]*
            rsym[j, i[[2]], i[[4]], {v6, v7}]*
            fsym[i[[1]], i[[3]], i[[2]], i[[4]], j, i[[6]], {v5, v7, i[[7, 3]], i[[7, 4]]}]
           , {j, Cases[irreps,  x_ /; MemberQ[fusion[i[[1]], i[[3]]], x] &&
             MemberQ[fusion[i[[2]], x], i[[4]]]]}
           , {v5, nv[i[[1]], i[[3]], j]}, {v6, nv[i[[2]], j, i[[4]]]}, {v7, nv[i[[2]], j, i[[4]]]}]) , 10^(-20) ]
       
       , {i, flist}]
      
      , 0];
    
    goodposonephase = 
     Position[temphex, x_Complex + c1_Complex phase[y1___]] // Flatten;
    
    If[Length[goodposonephase] > 0, phaseeqleft = True];
    
    ];
   
   
   firstlinpos = 
    Position[temphex, x_ /; FreeQ[x, phase[___]^2, Infinity], 1, 1, Heads -> False] // Flatten;
   
   If[temphex != {},
    If[
      firstlinpos == {},
      Print["There are variables phase[___] left, but no linear equations!"];,
      firstlinpos = firstlinpos[[1]];
      lineqleft = True;
      ];
    ];
   
   While[
    lineqleft,
    
    lineqleft = False;
    
    solphase = 
     Solve[temphex[[firstlinpos]] == 0, 
      Variables[temphex[[firstlinpos]]][[1]]];
    If[Length[solphase] != 1, 
     Print["There is more than one solution for the phases! Something went wrong!"]];
    solphase = Chop[ solphase[[1]] , 10^(-(Max[10, precision - 20])) ];
    
    Do[rsym[Sequence @@ rlist[[i]]] = Chop[ rsym[Sequence @@ rlist[[i]]] /. solphase , 10^(-(Max[10, precision - 20])) ];
    , {i, 1, Length[rlist]}];
    
    temphex =
     DeleteCases[
      Table[
       Chop[(Sum[rsym[i[[1]], i[[2]], i[[5]], {i[[7, 1]], v8}]*
             fsym[i[[1]], i[[2]], i[[3]], i[[4]], i[[5]], i[[6]], {v8, i[[7, 2]], v9, i[[7, 4]]}]*
             rsym[i[[3]], i[[2]], i[[6]], {v9, i[[7, 3]]}]
             , {v8, nv[i[[1]], i[[2]], i[[5]]]}, {v9,  nv[i[[3]], i[[2]], i[[6]]]}]
           -
           Sum[fsym[i[[2]], i[[1]], i[[3]], i[[4]], i[[5]], j, {i[[7, 1]], i[[7, 2]], v5, v6}]*
             rsym[j, i[[2]], i[[4]], {v6, v7}]*
             fsym[i[[1]], i[[3]], i[[2]], i[[4]], j, i[[6]], {v5, v7, i[[7, 3]], i[[7, 4]]}]
            , {j, Cases[irreps, x_ /; MemberQ[fusion[i[[1]], i[[3]]], x] &&
              MemberQ[fusion[i[[2]], x], i[[4]]]]}
            , {v5, nv[i[[1]], i[[3]], j]}, {v6, nv[i[[2]], j, i[[4]]]}, {v7, nv[i[[2]], j, i[[4]]]}]) , 10^(-20) ]
       
       , {i, flist}]
      
      , 0];
    
    firstlinpos = Position[temphex, x_ /; FreeQ[x, phase[___]^2, Infinity], 1, 1, Heads -> False] // Flatten;
    If[temphex != {},
     If[
       firstlinpos == {},
       Print["There are variables phase[___] left, but no linear equations!"];,
       firstlinpos = firstlinpos[[1]];
       lineqleft = True;
       ];
     ];
    
    
    ];
   
   
   
   (* Calculating the inverses of the R-symbols, 
   NOT assuming that the R-matrices are unitairy *)
   Do[
    rmat[Sequence @@ rm] = 
     Table[rsym[Sequence @@ rm[[1 ;; 3]], {v1, v2}]
     , {v1, nv[Sequence @@ rm[[1 ;; 3]]]}, {v2, nv[Sequence @@ rm[[1 ;; 3]]]}];
    rmatinv[Sequence @@ rm] = rmat[Sequence @@ rm] // Inverse;
    Do[
     rsyminv[Sequence @@ rm, {v1, v2}] = rmatinv[Sequence @@ rm][[v1, v2]];
     , {v1, 1, nv[Sequence @@ rm]}, {v2, 1, nv[Sequence @@ rm]}];
   , {rm, rmatlist}];
   
   
   rmatunitary = And @@ Table[
       ( Chop[ rmat[Sequence @@ rm].Conjugate[Transpose[rmat[Sequence @@ rm]]] - IdentityMatrix[nv[Sequence @@ rm]] , 10^(-20) ] // Flatten // Union) == {0}
       , {rm, rmatlist}] &&
       And @@ Table[(Chop[ Conjugate[Transpose[rmat[Sequence @@ rm]]].rmat[Sequence @@ rm] - IdentityMatrix[nv[Sequence @@ rm]] , 10^(-20) ] // Flatten // Union) == {0}
       , {rm, rmatlist}];
   
   rmatdiagonal = And @@ Table[
      (Chop[ (rmat[Sequence @@ rm] - DiagonalMatrix[Diagonal[rmat[Sequence @@ rm]]]) , 10^(-20) ] // Flatten // Union) == {0}
      , {rm, rmatlist}];
   
   (* Calculating the eigenvalues of the R-matrices, 
   to check if they can be made unitary if they are not already *)
   
   rmatevallist = Table[
       N[rmat[Sequence @@ rm] , precision ]// Eigenvalues
       , {rm, rmatlist}] // Flatten;
   
   rmatevalabslist = Abs /@ rmatevallist;
   
   If[
    (Chop[ (rmatevalabslist - 1) , 10^(-20) ] // Union) == {0},
    rmatevalsarephases = True;,
    rmatevalsarephases = False;
    ];
   
   rmatevalarglist = Rationalize[1/Pi (Arg /@ rmatevallist)];
   rmatevalarglistunion = rmatevalarglist // Union;
   
   rmatevalargmaxdenom = (Denominator /@ rmatevalarglist) // Max;
   
   
   
   
   ];(* End of constructrsymbols[] *)


checkhexagon[] := Module[{maxdev, tempdev},
   
   hexholds = True;
   hexrundecidable = False;
   hexrinvundecidable = False;
   maxdev = 0;
   
   Do[
    
    If[
      Chop[
        Sum[rsym[i[[1]], i[[2]], i[[5]], {i[[7, 1]], v8}]*
           fsym[i[[1]], i[[2]], i[[3]], i[[4]], i[[5]], i[[6]], {v8, i[[7, 2]], v9, i[[7, 4]]}]*
           rsym[i[[3]], i[[2]], i[[6]]
           , {v9, i[[7, 3]]}], {v8, nv[i[[1]], i[[2]], i[[5]]]}, {v9, nv[i[[3]], i[[2]], i[[6]]]}]
           -
         Sum[fsym[i[[2]], i[[1]], i[[3]], i[[4]], i[[5]], j, {i[[7, 1]], i[[7, 2]], v5, v6}]*
           rsym[j, i[[2]], i[[4]], {v6, v7}]*
           fsym[i[[1]], i[[3]], i[[2]], i[[4]], j, i[[6]], {v5, v7, i[[7, 3]], i[[7, 4]]}]
          , {j, Cases[irreps, x_ /; MemberQ[fusion[i[[1]], i[[3]]], x] &&
           MemberQ[fusion[i[[2]], x], i[[4]]]]}
          , {v5, nv[i[[1]], i[[3]], j]}, {v6, nv[i[[2]], j, i[[4]]]}, {v7, nv[i[[2]], j, i[[4]]]}]
        , 10^(-(Max[10, precision - 20]))] == 0, Null,
      
      hexholds = False;
      tempdev = Abs[
        Sum[rsym[i[[1]], i[[2]], i[[5]], {i[[7, 1]], v8}]*
           fsym[i[[1]], i[[2]], i[[3]], i[[4]], i[[5]], i[[6]], {v8, i[[7, 2]], v9, i[[7, 4]]}]*
           rsym[i[[3]], i[[2]], i[[6]]
           , {v9, i[[7, 3]]}], {v8, nv[i[[1]], i[[2]], i[[5]]]}, {v9, nv[i[[3]], i[[2]], i[[6]]]}]
           -
         Sum[fsym[i[[2]], i[[1]], i[[3]], i[[4]], i[[5]], j, {i[[7, 1]], i[[7, 2]], v5, v6}]*
           rsym[j, i[[2]], i[[4]], {v6, v7}]*
           fsym[i[[1]], i[[3]], i[[2]], i[[4]], j, i[[6]], {v5, v7, i[[7, 3]], i[[7, 4]]}]
          , {j, Cases[irreps, x_ /; MemberQ[fusion[i[[1]], i[[3]]], x] &&
            MemberQ[fusion[i[[2]], x], i[[4]]]]}
          , {v5, nv[i[[1]], i[[3]], j]}, {v6, nv[i[[2]], j, i[[4]]]}, {v7, nv[i[[2]], j, i[[4]]]}]
        
        ];
      
      If[tempdev > maxdev, maxdev = tempdev];
      ,
      If[Not[hexrundecidable], 
        Print["At least one of the hexagon equations for R is not decidable! Something went wrong :-("];
        hexrundecidable = True;];
      ];
    
    
    , {i, flist}];
   
   (* Checking the hexagon for the inverses of the R-matrices, 
   NOT assuming the R-matrices are unitary *)
   Do[
    
    If[
      Chop[
        Sum[rsyminv[i[[1]], i[[2]], i[[5]], {i[[7, 1]], v8}]*
           fsym[i[[1]], i[[2]], i[[3]], i[[4]], i[[5]], i[[6]], {v8, i[[7, 2]], v9, i[[7, 4]]}]*
           rsyminv[i[[3]], i[[2]], i[[6]], {v9, i[[7, 3]]}]
           , {v8, nv[i[[1]], i[[2]], i[[5]]]}, {v9, nv[i[[3]], i[[2]], i[[6]]]}]
           -
         Sum[fsym[i[[2]], i[[1]], i[[3]], i[[4]], i[[5]], j, {i[[7, 1]], i[[7, 2]], v5, v6}]*
           rsyminv[j, i[[2]], i[[4]], {v6, v7}]*
           fsym[i[[1]], i[[3]], i[[2]], i[[4]], j, i[[6]], {v5, v7, i[[7, 3]], i[[7, 4]]}]
          , {j, Cases[irreps, x_ /; MemberQ[fusion[i[[1]], i[[3]]], x] &&
            MemberQ[fusion[i[[2]], x], i[[4]]]]}
          , {v5, nv[i[[1]], i[[3]], j]}, {v6, nv[i[[2]], j, i[[4]]]}, {v7, nv[i[[2]], j, i[[4]]]}]
        , 10^(-(Max[10, precision - 20]))] == 0, Null,
      
      hexholds = False;
      tempdev = Abs[
        Sum[rsyminv[i[[1]], i[[2]], i[[5]], {i[[7, 1]], v8}]*
           fsym[i[[1]], i[[2]], i[[3]], i[[4]], i[[5]], i[[6]], {v8, i[[7, 2]], v9, i[[7, 4]]}]*
           rsyminv[i[[3]], i[[2]], i[[6]], {v9, i[[7, 3]]}]
           , {v8, nv[i[[1]], i[[2]], i[[5]]]}, {v9, nv[i[[3]], i[[2]], i[[6]]]}]
           -
         Sum[fsym[i[[2]], i[[1]], i[[3]], i[[4]], i[[5]], j, {i[[7, 1]], i[[7, 2]], v5, v6}]*
           rsyminv[j, i[[2]], i[[4]], {v6, v7}]*
           fsym[i[[1]], i[[3]], i[[2]], i[[4]], j, i[[6]], {v5, v7, i[[7, 3]], i[[7, 4]]}]
          , {j, Cases[irreps, x_ /; MemberQ[fusion[i[[1]], i[[3]]], x] &&
            MemberQ[fusion[i[[2]], x], i[[4]]]]}
            , {v5, nv[i[[1]], i[[3]], j]}, {v6, nv[i[[2]], j, i[[4]]]}, {v7, nv[i[[2]], j, i[[4]]]}]
        
        ];
      
      If[tempdev > maxdev, maxdev = tempdev];
      ,
      If[Not[hexrinvundecidable],
      Print["At least one of the hexagon equations for R is not decidable! Something went wrong :-("];
      hexrinvundecidable = True;];
      
      ];
    
    
    , {i, flist}];
   

   
   If[hexholds && Not[hexrundecidable] && Not[hexrinvundecidable],
    Print["The hexagon equations are satisfied :-)"];
    rsymbolscalculated = True;,
    Print["The hexagon equations are not satisfied :-(, the maximum deviation is: ", maxdev, " something went wrong..."];
    ];
   
   If[
    rmatunitary,
    Print["The R-matrices are unitary."];
    If[
     rmatdiagonal,
     Print["The R-matrices are diagonal."];
     Print["Thus all the R-symbols are phases. The largest denominator s in R^{a,b}_c = e^(i\[Pi] r/s) one encounters is: ", rmatevalargmaxdenom];
     ,
     Print["The R-matrices are not all diagonal."];
     Print["All the eigenvalues of the R-matrices are phases, because they are unitary. The largest denominator s in R^{a,b}_c = e^(i\[Pi] r/s) one encounters after diagonalization is: ", 
       rmatevalargmaxdenom];
     If[pentagontobechecked,
      Print["The R-matrices can be diagonalized by running diagonalizermatrices[].\n This will change both F- and R-symbols, so by running diagonalizermatrices[], the pentagon and hexagon equations will be checked again."];,
      Print["The R-matrices can be diagonalized by running diagonalizermatrices[].\n This will change both F- and R-symbols, so by running diagonalizermatrices[], the hexagon equations will be checked again, but not the pentagon equations, because you opted not to do so."];
      ];
     ];
    ];
   
   If[
    Not[rmatunitary],
    Print[
     "The R-matrices are not all unitary (at least in the basis used here)."];
    If[rmatevalsarephases,
     Print["All the eigenvalues of the R-matrices are phases. The largest denominator s in R^{a,b}_c = e^(i\[Pi] r/s) one encounters after diagonalization is: ", 
       rmatevalargmaxdenom];
     If[pentagontobechecked,
      Print["The R-matrices can be diagonalized by running diagonalizermatrices[].\n This will change both F- and R-symbols, so by running diagonalizermatrices[], the pentagon and hexagon equations will be checked again."];,
      Print["The R-matrices can be diagonalized by running diagonalizermatrices[].\n This will change both F- and R-symbols, so by running diagonalizermatrices[], the hexagon equations will be checked again, but not the pentagon equations, because you opted not to do so."];
     ];
     ,
     Print["NOT all the eigenvalues of the R-matrices are phases. One should really check if this is reasonable!"]
     ];
    ];
    
    If[hexholds && Not[hexrundecidable] && Not[hexrinvundecidable] && Not[recheck],
    Print["You can proceed to calculate the modular data :-)"];
    ];    
             
   ];(* End of checkhexagon[] *)


calculatersymbols[] := With[{},
   
   If[Not[typeranklevelrootinitok],
   Print["The type of algebra, rank, level and/or rootfactor were not \
(correctly) initialized, please do so first, followed by calculating \
the F-symbols, before calculating the R-symbols."];
   ];

  If[typeranklevelrootinitok && Not[fsymbolscalculated],
  Print["The F-symbols were not calculated. Please do so first, \
before calculating the R-symbols."];
  ];

   If[typeranklevelrootinitok && fsymbolscalculated,
   
   Print["Constructing the R-symbols..."];
   
   constructrsymbols[];
   
   Print["Checking the ", 2*Length[flist]," hexagon equations..."];
   
   checkhexagon[];
   
   Print["Done :-)"];
   ];
   
  ];


(* ::Subsection::Closed:: *)
(*Routine to diagonalize the R - matrices*)


diagonalizermatrices[] := With[{},
   
  If[typeranklevelrootinitok && fsymbolscalculated && rsymbolscalculated,
   
   If[Not[rmatdiagonal],
   
   Print["Diagonalizing the R-matrices..."];
   
   recheck = True;
   
   rmatdiagonallist = DeleteCases[Table[
      If[ Chop[ rmat[Sequence @@ rm] , 10^(-(Max[10, precision - 20])) ] // DiagonalMatrixQ , rm]
      , {rm, rmatlist}], Null];
   
   
   rmatnondiagonallist = DeleteCases[Table[
      If[Not[ Chop[ rmat[Sequence @@ rm] , 10^(-(Max[10, precision - 20])) ] // DiagonalMatrixQ], rm]
      , {rm, rmatlist}], Null];
   
   Do[
    umat[Sequence @@ rm] = IdentityMatrix[nv[Sequence @@ rm]];
    umatinv[Sequence @@ rm] = umat[Sequence @@ rm];
    , {rm, rmatdiagonallist}];
   
   Do[
    umat[Sequence @@ rm] = (Chop[ rmat[Sequence @@ rm] // Eigensystem , 10^(-(Max[10, precision - 20])) ])[[2]] // Transpose;
    umatinv[Sequence @@ rm] = Inverse[umat[Sequence @@ rm]];
    , {rm, rmatnondiagonallist}];
   
   Do[
    rsymold[Sequence @@ rs] = rsym[Sequence @@ rs]
    , {rs, rlist}];
   
   Do[
    fsymold[Sequence @@ fs] = fsym[Sequence @@ fs]
    , {fs, flist}];
   
   Do[
    rmatold[Sequence @@ rm] = rmat[Sequence @@ rm]
    , {rm, rmatlist}];
   
   Do[
    fmatold[Sequence @@ fm] = fmat[Sequence @@ fm]
    , {fm, fmatlist}];
   
   Do[
    rsym[Sequence @@ rs] = Sum[
       umatinv[rs[[1]], rs[[2]], rs[[3]]][[rs[[4, 1]], v1]]*
        rsymold[rs[[1]], rs[[2]], rs[[3]], {v1, v2}]*
        umat[rs[[1]], rs[[2]], rs[[3]]][[v2, rs[[4, 2]]]]
       , {v1, 1, nv[rs[[1]], rs[[2]], rs[[3]]]}, {v2, 1, nv[rs[[1]], rs[[2]], rs[[3]]]}];
    , {rs, rlist}];
   
   Do[
    fsym[Sequence @@ fs] = Sum[
      umatinv[fs[[1]], fs[[2]], fs[[5]]][[fs[[7, 1]], v1]]*
       umatinv[fs[[5]], fs[[3]], fs[[4]]][[fs[[7, 2]], v2]]*
       fsymold[fs[[1]], fs[[2]], fs[[3]], fs[[4]], fs[[5]], fs[[6]], {v1, v2, v3, v4}]*
       umat[fs[[2]], fs[[3]], fs[[6]]][[v3, fs[[7, 3]]]]*
       umat[fs[[1]], fs[[6]], fs[[4]]][[v4, fs[[7, 4]]]]
      , {v1, 1, nv[fs[[1]], fs[[2]], fs[[5]]]}
      , {v2, 1, nv[fs[[5]], fs[[3]], fs[[4]]]}
      , {v3, 1, nv[fs[[2]], fs[[3]], fs[[6]]]}
      , {v4, 1, nv[fs[[1]], fs[[6]], fs[[4]]]}]
    , {fs, flist}];
   
   Do[
    fmat[Sequence @@ fm] =
      Flatten[
       Table[
        fsym[fm[[1]], fm[[2]], fm[[3]], fm[[4]], e,  f, {v1, v2, v3, v4}]
        ,{e, Cases[fusion[fm[[1]], fm[[2]]], x_ /; MemberQ[fusion[x, fm[[3]]], fm[[4]]]]}
        ,{f, Cases[fusion[fm[[2]], fm[[3]]], x_ /; MemberQ[fusion[fm[[1]], x], fm[[4]]]]}
        ,{v1, nv[fm[[1]], fm[[2]], e]}
        ,{v2, nv[e, fm[[3]], fm[[4]]]}
        ,{v3, nv[fm[[2]], fm[[3]], f]}
        ,{v4, nv[fm[[1]], f, fm[[4]]]}
        ]
       , {{1, 3, 4}, {2, 5, 6}}];
    , {fm, fmatlist}];
     
   Do[
    rmat[Sequence @@ rm] = 
     Table[rsym[Sequence @@ rm[[1 ;; 3]], {v1, v2}]
     , {v1, nv[Sequence @@ rm[[1 ;; 3]]]}
     , {v2, nv[Sequence @@ rm[[1 ;; 3]]]}];
    
    rmatinv[Sequence @@ rm] = rmat[Sequence @@ rm] // Inverse;
    
    Do[
     rsyminv[Sequence @@ rm, {v1, v2}] = 
       rmatinv[Sequence @@ rm][[v1, v2]];
     , {v1, 1, nv[Sequence @@ rm]}, {v2, 1, nv[Sequence @@ rm]}];
    
    , {rm, rmatlist}];
   
   
   rmatunitary = And @@ Table[
       ( Chop[ rmat[Sequence @@ rm].Conjugate[Transpose[rmat[Sequence @@ rm]]] - IdentityMatrix[nv[Sequence @@ rm]] , 10^(-20) ] // Flatten // Union) == {0}
       , {rm, rmatlist}] &&
       And @@ Table[(Chop[ Conjugate[Transpose[rmat[Sequence @@ rm]]].rmat[Sequence @@ rm] - IdentityMatrix[nv[Sequence @@ rm]] , 10^(-20) ] // Flatten // Union) == {0}
       , {rm, rmatlist}];
   
   rmatdiagonal = And @@ Table[
      (Chop[ (rmat[Sequence @@ rm] - DiagonalMatrix[Diagonal[rmat[Sequence @@ rm]]]) , 10^(-20) ] // Flatten // Union) == {0}
      , {rm, rmatlist}];
   
   rmatevallist = Table[
       N[rmat[Sequence @@ rm] , precision ]// Eigenvalues
       , {rm, rmatlist}] // Flatten;
   
   rmatevalabslist = Abs /@ rmatevallist;
   
   If[
    (Chop[ (rmatevalabslist - 1) , 10^(-20) ] // Union) == {0},
    rmatevalsarephases = True;,
    rmatevalsarephases = False;
    ];
   
   rmatevalarglist = Rationalize[1/Pi (Arg /@ rmatevallist)];
   rmatevalarglistunion = rmatevalarglist // Union;
   
   rmatevalargmaxdenom = (Denominator /@ rmatevalarglist) // Max;
   
   checkpentagon[];
   
   Print["Re-checking the ", 2*Length[flist]," hexagon equations..."];
   checkhexagon[];
   
   If[
    (Not[pentagontobechecked] || (pentholds && Not[pentundecidable])) && hexholds && Not[hexrundecidable] && Not[hexrinvundecidable],
    Clear[fsymold, rsymold, fmatold, rmatold, umat, umatinv];
    ];
   
   Print["Done :-)"];
   
   ,
   Print["The R-matrices are already diagonal, so there's no need to diagonalize them."];
   Print["Done :-)"];
   ];
   ,
   Print["The R-symbols have not been calculated, please do so first!"];
   
   ];
   
   ];


(* ::Subsection::Closed:: *)
(*Command to calculate the modular data*)


calculatemodulardata[] := Module[{},
  Clear[qd, dual, selfdual, pivot, theta, hvalue, frobschur];
  
  If[Not[typeranklevelrootinitok],
  Print["The type of algebra, rank, level and/or rootfactor were not \
(correctly) initialized, please do so first, followed by calculating \
the F- and R-symbols, before calculating the modular data."];
  ];

If[typeranklevelrootinitok && Not[fsymbolscalculated],
  Print["The F-symbols were not calculated. Please do so first, \
followed by calculating the R-symbols, before calculating the modular \
data."];
  ];

If[typeranklevelrootinitok && fsymbolscalculated && 
   Not[rsymbolscalculated],
  Print["The R-symbols were not calculated. Please do so first, \
before calculating the modular data."];
  ];
  
  If[typeranklevelrootinitok&&fsymbolscalculated&&rsymbolscalculated,

  
  numofirreps = Length[irreps];
  numposroots = Position[roots, Table[0, {i, 1, rank}], 1][[1, 1]] - 1;
  posroots = roots[[1 ;; numposroots]];
  
  qdimvec = Table[
    Chop[ N[ Product[
       nq[tmax (ir + rho).qfm.posroots[[pr]], 1] / nq[ tmax (rho).qfm.posroots[[pr]], 1]
       , {pr, 1, numposroots}] , precision ] , 10^(-(Max[10, precision - 20])) ]
    , {ir, irreps}];
    
  Do[
   qd[irreps[[i]]] = qdimvec[[i]]
   , {i, 1, Length[irreps]}];
   
  qdtot2 = Sum[qdimvec[[i]]^2, {i, 1, Length[irreps]}];
  qdtot = Sqrt[qdtot2];
  qdimspositive = And @@ (# > 0 & /@ qdimvec);
  
  fpdimvec = 
   Table[
    (Cases[Chop[ (N[ nmat[ir] , precision ] // Eigenvalues) , 10^(-(Max[10, precision - 20])) ], x_ /; x \[Element] Reals] // Sort)[[-1]]
   , {ir, irreps}];
  
  irrepsdual = 
   Table[
   irreps[[Position[nmat[irreps[[i]]], x_ /; x[[1]] == 1][[1, 1]]]]
      , {i, 1, Length[irreps]}] // Quiet;
       
  Do[
   dual[irreps[[i]]] = irrepsdual[[i]]
   , {i, 1, Length[irreps]}];
  
  selfdualvec = 
   Table[irreps[[i]] == irrepsdual[[i]], {i, 1, numofirreps}];
  Do[
   selfdual[irreps[[i]]] = selfdualvec[[i]]
   , {i, 1, numofirreps}];
  
  qdim1overfvec = 
   Table[
    N[ 1/fsym[irreps[[i]], irrepsdual[[i]], irreps[[i]], irreps[[i]], irreps[[1]], irreps[[1]], {1, 1, 1, 1}] , precision ]
   , {i, 1, Length[irreps]}];
  
  pivotlist = 
   Table[qdimvec[[i]]/qdim1overfvec[[i]], {i, 1, Length[irreps]}];

  Do[pivot[irreps[[i]]] = pivotlist[[i]], {i, 1, Length[irreps]}];

  
  pivoteqnok = (Table[
        Chop[ N[ pivot[ir1] pivot[ir2]/pivot[ir3] -
           Sum[fsym[ir1, ir2, dual[ir3], irreps[[1]], ir3, dual[ir1], {v1, 1, v2, 1}]*
             fsym[ir2, dual[ir3], ir1, irreps[[1]], dual[ir1], dual[ir2], {v2, 1, v3, 1}]*
             fsym[dual[ir3], ir1, ir2, irreps[[1]], dual[ir2], ir3, {v3, 1, v1, 1}]
            , {v2, 1, nv[ir2, dual[ir3], dual[ir1]]}, {v3, 1, nv[dual[ir3], ir1, dual[ir2]]}] , precision ] , 10^(-20) ] 
        , {ir1, irreps}, {ir2, irreps}, {ir3, fusion[ir1, ir2]}, {v1, 1, nv[ir1, ir2, ir3]}] // Flatten // Union) == {0};
  
  If[Not[MemberQ[pivotlist, x_ /; Not[Chop[x - 1, 10^(-20)] == 0 || Chop[x + 1, 10^(-20)] == 0 ]]],
   pivotlist = Round[pivotlist];
  ];
  
 
  thetalist = Table[
    1/qd[ir] Sum[qd[ir1] rsym[ir, ir, ir1, {v, v}], {ir1, fusion[ir, ir]}, {v, 1, nv[ir, ir, ir1]}]
    , {ir, irreps}];
    
  Do[
   theta[irreps[[i]]] = thetalist[[i]]
   , {i, 1, Length[irreps]}];

  hlist = 
   Table[Mod[Rationalize[Chop[1/(2 Pi I) Log[thetalist[[i]]], 10^(-(Max[10, precision - 20])) ]], 1]
   , {i, 1, Length[irreps]}];
  
  Do[
   hvalue[irreps[[i]]] = hlist[[i]]
   , {i, 1, numofirreps}];
  
  frobschurlist = Chop[ Table[
     1/qdtot2 Sum[nv[ir, ir1, ir2] qd[ir1] qd[ir2] theta[ir1]^2/theta[ir2]^2, {ir1, irreps}, {ir2, fusion[ir, ir1]}]
     , {ir, irreps}] , 10^(-(Max[10, precision - 20])) ];
  
  Do[
   frobschur[irreps[[i]]] = frobschurlist[[i]]
   , {i, 1, numofirreps}];
   
  If[Not[MemberQ[frobschurlist, x_ /; Not[Chop[x - 1, 10^(-20)] == 0 || Chop[x + 1, 10^(-20)] == 0 || Chop[x, 10^(-20)] == 0]]],
   frobschurlist = Round[frobschurlist];
  ];
  
  smat = Chop[ 1/(qdtot) Table[ 
      Sum[qd[ir3] theta[ir3] * nv[dual[ir1], ir2, ir3]/(theta[dual[ir1]] * theta[ir2]), {ir3, fusion[dual[ir1], ir2]}]
        , {ir1, irreps}, {ir2, irreps}] , 10^(-(Max[10, precision - 20])) ];
  
  cmat = SparseArray[
    Table[{i, Position[irrepsdual, irreps[[i]]][[1, 1]]} -> 1, {i, 1, numofirreps}]
      ] // Normal;
  
  tmat = DiagonalMatrix[thetalist];
  
  modular = (Chop[ smat.ConjugateTranspose[smat] - IdentityMatrix[numofirreps] , 10^(-20) ] // Flatten // Union) == {0};
  
  pplus = Sum[qd[ir]^2 theta[ir], {ir, irreps}];
  pminus = Sum[qd[ir]^2 /theta[ir], {ir, irreps}];
  modular2 = (Chop[ pplus pminus - qdtot2 , 10^(-20) ]) == 0;
  If[modular != modular2, Print["The two ways of determining modularity do NOT agree!"]];
  If[modular,
   centralcharge = Chop[ 8/(2 Pi I) Log[pplus/qdtot] , 10^(-(Max[10, precision - 20])) ] // Rationalize;
   centralcharge = Mod[centralcharge, 8];
   ];
  
  If[modular,
  modularrelationsok =
   (( Chop[ MatrixPower[smat.tmat, 3] - Exp[2 Pi I/8*centralcharge] smat.smat // Flatten , 10^(-20) ] // Union) == {0}) &&
   (( Chop[ smat.smat - cmat , 10^(-20) ] // Flatten // Union) == {0});
  ];
  
  
  Print["The labels of the irreps are: ", irreps];
  Print["Are the particles selfdual: ", selfdualvec];
  Print["The pivotal structure is: ", pivotlist];
  If[pivoteqnok,
   Print["The pivotal equations are satisfied :-)"];,
   Print["The pivotal equations are NOT satisfied :-("];,
   Print["The pivotal equations could not be checked."];];
  Print["The Frobenius-Schur indicators are: ", frobschurlist];
  Print["The Frobenius-Perron dimensions are: ", fpdimvec // N];
  Print["The quantum dimensions are: ", qdimvec // N];
  Print["The scaling dimensions modulo one (calculated from the twists) are: ", hlist];
  
  If[fmatunitary && Not[qdimspositive],
   Print["The f-matrices are all unitary, but the quantum dimensions are not all positive. Better check!"];
   ];

  If[Not[fmatunitary] && qdimspositive,
   Print["The f-matrices are not all unitary, but the quantum dimensions are all positive. Better check!"];
   ];
  
  If[
   qdimspositive,
   Print["The theory is unitary."];,
   Print["The theory is NOT unitary."];
   ];
  
  If[modular,
   Print["The theory is modular."];,
   Print["The theory is NOT modular."];,
   Print["The modularity of the theory could not be determined."];];
  
  If[modular && qdimspositive,
   Print["The central charge is: ", centralcharge, " (determined mod 8)"];
   ];
  If[modular && Not[qdimspositive],
   Print["The central charge is: ", centralcharge, " (determined mod 4)"];
   ];
  
  If[modular && qdimspositive,
   Print["The S-matrix is given by:"]; 
   Print[smat // N // MatrixForm];
   ];
  If[modular && Not[qdimspositive],
   Print["The S-matrix is given by (up to an overall sign):"];
   Print[smat // N // MatrixForm];
   ];
   
  If[modular && modularrelationsok,
   Print["The modular relations Exp[- 2 Pi I/8 centralcharge](S.T)^3 = S^2 = C are satisfied :-)"]
  ];
  
  If[modular && Not[modularrelationsok],
    Print["The modular relations Exp[- 2 Pi I/8 centralcharge](S.T)^3 = S^2 = C are not satisfied :-("]
  ];

  
  modulardatacalculated=True;
  
  Global`FPdimlist = fpdimvec;
  Global`qdimlist = qdimvec;
  Global`pivotlist = pivotlist;
  Global`thetalist = thetalist;
  Global`hlist = hlist;
  Global`FSlist = frobschurlist;
  Global`smat = smat;
  Global`tmat = tmat;
  Global`cmat = cmat;
  Global`centralcharge = centralcharge;
  Global`modular = modular;
  Global`unitary = qdimspositive;
  
  Print["Done :-)"];
  
  ];
  
 ];


(* ::Section::Closed:: *)
(*End `Private` Context*)


End[];


(* ::Section::Closed:: *)
(*End Package*)


EndPackage[]
