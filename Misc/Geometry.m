(* Mathematica Package 

Grab bag collection of geometrical code.

Nikhil Padmanabhan, Yale, Nov 2012

*)



BeginPackage["Misc`Geometry`"]
(* Exported symbols added here with SymbolName::usage *)  

wrapRA::usage = "wrapRA[ra] wraps the RA value to -Pi, Pi";
eulerRotationMatrix::usage = "eulerRotationMatrix[phi, theta, psi] computes a rotation matrix for phi, theta, psi";
convertSphericalToUnitVector::usage = 
	"convertSphericalToUnitVector[ra, dec] converts RA, Dec to x,y,z. Angles are assumed to be in radians";
convertUnitVectorToSpherical::usage = 
	"convertUnitVectorToSpherical[x,y,z] converts x,y,z to RA, Dec. The vector is normalized for safety";



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

Clear[convertSphericalToUnitVector, convertUnitVectorToSpherical];
convertSphericalToUnitVector[ra_, dec_] := {Cos[ra] Cos[dec], Sin[ra] Cos[dec], Sin[dec]};
convertUnitVectorToSpherical[x_, y_, z_] := With[{r = Sqrt[x^2+y^2+z^2]}, {ArcTan[x,y], Pi/2 - ArcCos[z/r]}];



End[] (* End Private Context *)

EndPackage[]