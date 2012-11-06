(* Mathematica Package 

Grab bag collection of geometrical code.

Nikhil Padmanabhan, Yale, Nov 2012

*)



BeginPackage["Misc`Geometry`"]
(* Exported symbols added here with SymbolName::usage *)  

eulerRotationMatrix::usage = "eulerRotationMatrix[phi, theta, psi] computes a rotation matrix for phi, theta, psi";



Begin["`Private`"] (* Begin Private Context *) 

eulerRotationMatrix[phi_, theta_, psi_] := Module[{b,c,d},
	b = RotationMatrix[psi, {0,0,1}];
	c = RotationMatrix[theta, {1,0,0}];
	d = RotationMatrix[phi, {0,0,1}];
	b.c.d
	];

End[] (* End Private Context *)

EndPackage[]