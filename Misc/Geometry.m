(* Mathematica Package 

Grab bag collection of geometrical code.

Nikhil Padmanabhan, Yale, Nov 2012

*)



BeginPackage["Misc`Geometry`"]
(* Exported symbols added here with SymbolName::usage *)  

wrapRA::usage = "wrapRA[ra] wraps the RA value to -Pi, Pi";
eulerRotationMatrix::usage = "eulerRotationMatrix[phi, theta, psi] computes a rotation matrix for phi, theta, psi";



Begin["`Private`"] (* Begin Private Context *) 

Clear[eulerRotationMatrix];
eulerRotationMatrix[phi_, theta_, psi_] := Module[{b,c,d},
	b = RotationMatrix[psi, {0,0,1}];
	c = RotationMatrix[theta, {1,0,0}];
	d = RotationMatrix[phi, {0,0,1}];
	b.c.d
	];

Clear[wrapRA];
wrapRA[ra_ /; (ra <= Pi) && (ra > -Pi)] := ra;
wrapRA[ra_ /; (ra > Pi) && (ra < 2 Pi)] := ra - 2 Pi;
wrapRA[ra_ /; (ra >=  2 Pi) || (ra <= -Pi)] := wrapRA[Mod[ra, 2 Pi]];

End[] (* End Private Context *)

EndPackage[]