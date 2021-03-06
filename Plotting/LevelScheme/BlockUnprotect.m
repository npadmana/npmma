(* ::Package:: *)

(************************************************************************)
(* This file was generated automatically by the Mathematica front end.  *)
(* It contains Initialization cells from a Notebook file, which         *)
(* typically will have the same name as this file except ending in      *)
(* ".nb" instead of ".m".                                               *)
(*                                                                      *)
(* This file is intended to be loaded into the Mathematica kernel using *)
(* the package loading commands Get or Needs.  Doing so is equivalent   *)
(* to using the Evaluate Initialization Cells menu command in the front *)
(* end.                                                                 *)
(*                                                                      *)
(* DO NOT EDIT THIS FILE.  This entire file is regenerated              *)
(* automatically each time the parent Notebook file is saved in the     *)
(* Mathematica front end.  Any changes you make to this file will be    *)
(* overwritten.                                                         *)
(************************************************************************)



(* :Title: BlockUnprotect *)
(* :Context: BlockUnprotect` *)
(* :Author: Mark A. Caprio, Center for Theoretical Physics, Yale University *)
(* :Summary: Dynamic scoping structure for options associated with a symbol. *)
(* :Copyright: Copyright 2005, Mark A. Caprio *)
(* :Package Version: 0.0 *)
(* :Mathematica Version: 4.0 *)
(* :History:
Adapted from BlockOptions December 1, 2005.
Context changed to LevelScheme`*.
*)


BeginPackage["LevelScheme`BlockUnprotect`"];


Unprotect[Evaluate[$Context<>"*"]];


BlockUnprotect::usage="BlockUnprotect[{symbol1,...},body] temporarily unprotects the given symbols while body is evaluated and then restores their prior protection status.";


BlockUnprotect::needlist="The first argument of BlockUnprotect must be a list of symbols.";BlockUnprotect::numargs="BlockUnprotect must be called with exactly two arguments.";


Begin["`Private`"];


(* Private copy of DoForEach, from package ForEach, version 1.0. *)


SetAttributes[DoForEach,HoldAll];
DoForEach[Expr_,{Var_Symbol,ValueSet_}]:=Module[
{i},

If[
Head[ValueSet]=!=List,
Message[DoForEach::notlist,ValueSet]
];
Do[
Block[
{Var=ValueSet[[i]]},
Expr
],
{i,1,Length[ValueSet]}
]

];
DoForEach[Expr_,{Var_Symbol,CountVar_Symbol,ValueSet_}]:=Module[
{i},

If[
Head[ValueSet]=!=List,
Message[DoForEach::notlist,ValueSet]
];

Block[
{CountVar},
Do[
CountVar=i;
Block[
{Var=ValueSet[[i]]},
Expr
],
{i,1,Length[ValueSet]}
]
]

];
DoForEach[Expr_,FirstIterator_List,RestIteratorSeq__List]:=DoForEach[DoForEach[Expr,RestIteratorSeq],FirstIterator];


SetAttributes[BlockUnprotect,HoldRest];


BlockUnprotect[x_/;!MatchQ[x,{___Symbol}],_]:=Message[BlockUnprotect::needlist];
BlockUnprotect[_]:=Message[BlockUnprotect::numargs];
BlockUnprotect[_,_,__]:=Message[BlockUnprotect::numargs];


BlockUnprotect[IdentifierList:{___Symbol},Body_]:=Module[
{Identifier,EvaluatedBody,IsProtected,Aborted},
AbortProtect[

(* Clear protection *)
DoForEach[
IsProtected[Identifier]=MemberQ[Attributes[Evaluate[Identifier]],Protected];
If[IsProtected[Identifier],Unprotect[Evaluate[Identifier]]],
{Identifier,IdentifierList}
];

(* Evaluate body *)
Aborted=False;
CheckAbort[
EvaluatedBody=Body,
Aborted=True
];

(* Restore protection *)
DoForEach[
If[IsProtected[Identifier],Protect[Evaluate[Identifier]]],
{Identifier,IdentifierList}
];
];

(* Return value *)
(* Passes through abort, and also explicitly returns $Aborted in case Abort[] is suppressed *)
If[Aborted,Abort[];$Aborted,EvaluatedBody]
];


End[];


Protect[Evaluate[$Context<>"*"]];
EndPackage[];



