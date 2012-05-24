BeginPackage["Cosmology`MassFunction`", 
             {"Cosmology`Defs`"}
            ];

(* You should put usage statements here *)
mass2rval::usage = "mass2rval[M, cosmo] converts mass (Msun) to Rval (Mpc)";
deltac::usage = "deltac is the spherical collapse overdensity; we keep this as a symbolic constant = 1.686";
ShethTormen::usage = "ShethTormen[v] is the Sheth-Tormen multiplicity function";
PressSchechter::usage = "PressSchechter[v] is the PS multiplicity function";
bias::usage = "bias[multiplicity, nu_] returns a bias given a multiplicity function";

(* The private section of the package is here *)
Begin["`Private`"];

mass2rval[M_, cosmo_?OptionQ] := Module[{rhobar}, 
   rhobar = OmegaM[1.0,cosmo]*CriticalDensity[1.0, cosmo];
   (3.0*M/(4.0*Pi*rhobar))^(1./3.)
];

(* Generic functional form *)
vfv[v_] := norm*(1 + (a*v^2)^(-p)) * Exp[- a*v^2/2] / Sqrt[a];
ShethTormen[v_] := vfv[v] /. {a -> 0.707, p-> 0.3, norm-> 1/(2*5.50221)};
PressSchechter[v_] := vfv[v]/.{a->1, p->0, norm-> 1/Sqrt[8 Pi]};

bias[multiplicity_, nu_] := Module[{x}, 
	1 + Simplify[D[Log[x * multiplicity[x]], x]] * (-x / deltac)/. {x-> nu}
];

End[];
EndPackage[];


