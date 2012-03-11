(* ::Package:: *)

BeginPackage["Cosmology`Defs`"]

z2a::usage = "Convert redshift to scale factor"
a2z::usage = "Convert scale factor into redshift"

Omegabh2::usage = "Physical density, baryons z=0"
OmegaMh2::usage = "Physical density, matter z=0"
OmegaKh2::usage = "Physical density, curvature z=0"
OmegaDEh2::usage = "Physical density, dark energy z=0"
ns::usage = "Primordial spectral index"
w0::usage = "Dark energy equation of state, w(a) = w0 + (1-a) wa"
wa::usage = "Dark energy equation of state, w(a) = w0 + (1-a) wa"

FoMSWG::usage = "The Figure of Merit Science Working Group cosmology, assuming w0 and wa"



Begin["`Private`"]

a2z[a_] := 1./a - 1;
z2a[z_] := 1./(1.+z); 

FoMSWG = {Omegabh2 -> 0.0227, OmegaMh2 -> 0.1326, OmegaKh2 -> 0.0, 
	  OmegaDEh2 -> 0.3844, ns-> 0.963, w0 -> -1.0, wa -> 0.0};

End[]

EndPackage[]
