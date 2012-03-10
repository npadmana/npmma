(* ::Package:: *)

(* Cosmological Distances -- based on Hogg 2000 *)

BeginPackage["Cosmology`Distances`"]

Needs["Cosmology`Defs`"]

Hubble::usage = 
"Hubble[a, cosmo] computes the Hubble parameter at a"
comdis::usage = 
"comdis[a, cosmo] computes the (line of sight) comoving distance to a"
propmotdis::usage = "propmotdis[a, cosmo] computes the (transverse) comoving distance to a"
angdis::usage = 
"angdis[a, cosmo] - Angular diameter distance to a"
lumdis::usage =
"lumdis[a, cosmo] - Luminosity distance to a"

Begin["`Private`"]



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

End[]
EndPackage[]
