(* ::Package:: *)

BeginPackage["Cosmology`Defs`"]

FoMSWG::usage = "The Figure of Merit Science Working Group cosmology, assuming w0 and wa"
z2a::usage = "Convert redshift to scale factor"
a2z::usage = "Convert scale factor into redshift"

FoMSWG = {Omegabh2 -> 0.0227, OmegaMh2 -> 0.1326, OmegaKh2 -> 0.0, 
	  OmegaDEh2 -> 0.3844, ns-> 0.963, w0 -> -1.0, wa -> 0.0};

Begin["`Private`"]
a2z[a_] := 1./a - 1;
z2a[z_] := 1./(1.+z); 

End[]

EndPackage[]
