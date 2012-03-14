(* ::Package:: *)

BeginPackage["Cosmology`Defs`"]

z2a::usage = "Convert redshift to scale factor"
a2z::usage = "Convert scale factor into redshift"
OmegaM::usage = "Calculate OmegaM[a, cosmo] at a"

Omegabh2::usage = "Physical density, baryons z=0"
OmegaMh2::usage = "Physical density, matter z=0"
OmegaKh2::usage = "Physical density, curvature z=0"
OmegaDEh2::usage = "Physical density, dark energy z=0"
ns::usage = "Primordial spectral index"
w0::usage = "Dark energy equation of state, w(a) = w0 + (1-a) wa"
wa::usage = "Dark energy equation of state, w(a) = w0 + (1-a) wa"
TCMB::usage = "Temperature of the CMB, Kelvin"

FoMSWG::usage = "The Figure of Merit Science Working Group cosmology, assuming w0 and wa"



(* Cosmological Distances -- based on Hogg 2000 *)

Hubble::usage = 
"Hubble[a, cosmo] computes the Hubble parameter at a in units of 100 km/s/Mpc"
comdis::usage = 
"comdis[a, cosmo] computes the (line of sight) comoving distance to a; units of c/100 Mpc"
propmotdis::usage = "propmotdis[a, cosmo] computes the (transverse) comoving distance to a; units of c/100 Mpc"
angdis::usage = 
"angdis[a, cosmo] - Angular diameter distance to a; units of c/100 Mpc"
lumdis::usage =
"lumdis[a, cosmo] - Luminosity distance to a; units of c/100 Mpc"

(*----------------------------------*)
Begin["`Private`"]

(* Define some simple pieces of code here *)
a2z[a_] := 1./a - 1;
z2a[z_] := 1./(1.+z); 
OmegaM[a_, cosmo_?OptionQ] := Module[{h2}, 
  h2 = Hubble[a,cosmo]^2;
  OmegaMh2/a^3/h2 /. cosmo
]

(*----------------------------------*)
(* Actual specific cosmological definitions go here *)

FoMSWG = {Omegabh2 -> 0.0227, OmegaMh2 -> 0.1326, OmegaKh2 -> 0.0, 
	  OmegaDEh2 -> 0.3844, ns-> 0.963, w0 -> -1.0, wa -> 0.0, 
          TCMB -> 2.726};

(*----------------------------------*)
(* Distances go here *)

(* The Hubble Parameter *)
Hubble[a_, cosmo_?OptionQ] := Module[{h2}, 
   h2 = (OmegaMh2 /a^3 + OmegaKh2/a^2 + 
 OmegaDEh2 * Exp[3*(-wa + a * wa - (1+w0+wa)Log[a])])/.cosmo;
   Sqrt[h2]];

(* Distance Measures -- Hogg 2000 *)
comdis[a_, cosmo_?OptionQ]:= Module[{f}, 
f[x_] = 1./(Hubble[x, cosmo] * x^2);
NIntegrate[f[x], {x, a, 1.0}]];

propmotdis[a_, cosmo_?OptionQ] := Module[{ok, dc},
dc  = comdis[a, cosmo];
ok = OmegaKh2 /. cosmo;
Piecewise[ {
{1./Sqrt[ok] * Sinh[Sqrt[ok]*dc],ok > 0}, 
{dc, ok == 0}, 
{1./Sqrt[-ok] * Sinh[Sqrt[-ok]*dc], ok < 0}}]];
angdis[a_, cosmo_?OptionQ] := propmotdis[a, cosmo] * a;
lumdis[a_, cosmo_?OptionQ] := propmotdis[a, cosmo]/a;

(*----------------------------------*)
End[]


(*----------------------------------*)
EndPackage[]
