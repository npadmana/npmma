(* Formulae in Eisenstein & Hu ApJ, 1998 *)

BeginPackage["Cosmology`EH98`"];

zeqEH::usage = "zeqEH[cosmo] returns the equality redshift (Eq. 2, EH98)";
keqEH::usage = "keqEH[cosmo] returns k_eq in Mpc^-1 (Eq. 3, EH98)";
zdecEH::usage = "zdecEH[cosmo] returns the decoupling redshift (Eq. 4, EH98)";
rsEH::usage = "rsEH[cosmo] returns the sound horizon in Mpc(Eq.6, EH98)";
tkEH::usage = "tkEH[k, cosmo] returns the transfer fn at k (in h^-1 Mpc)";
pkEH::usage = "pkEH[k, ns, deltah2, cosmo] returns the no-wiggle power spectrum at k.";

Needs["Cosmology`Defs`"];

Begin["`Private`"];

(* Eq. 2 *) 
zeqEH[cosmo_?OptionQ] := Module[{om,t0},
            om = OmegaMh2 /. cosmo;
            t0 = (TCMB/2.7) /. cosmo;
            2.5 * 10.0^4 * om * t0^-4
            ]; 

(* Eq 3 *)

keqEH[cosmo_?OptionQ] := Module[{om, t0},
            om = OmegaMh2 /. cosmo;
            t0 = (TCMB/2.7) /. cosmo;
            7.46 * 10^-2 * om * t0^-2
            ];

(* Eq 4 *) 

zdecEH[cosmo_?OptionQ] := Module[{om,ob,b1, b2}, 
    om = OmegaMh2 /. cosmo;
    ob = Omegabh2 /. cosmo;
    b2 = 0.238*om^0.223;
    b1 = 0.313*om^-0.419 * (1.0 + 0.607 * om^0.674);
    1291 * om^0.251 * (1.0+b1*ob^b2)/(1.0 + 0.659*om^0.828)
    ];


Rsound[z_, cosmo_?OptionQ] := Module[{ob, t0}, 
    t0 = (TCMB/2.7) /. cosmo;
    ob = Omegabh2 /. cosmo;
    31.5*ob*t0^-4 / (z/1000.0)
    ];

rsEH[cosmo_?OptionQ] := Module[{keq, zeq, zd, req, rd}, 
  keq = keqEH[cosmo];
  zeq = zeqEH[cosmo]; zd = zdecEH[cosmo];
  req = Rsound[zeq, cosmo]; rd = Rsound[zd, cosmo];
  (2./3/keq) * Sqrt[6./req] * Log[ (Sqrt[1.0+ rd] + Sqrt[rd + req])/(1.0+Sqrt[req])]
  ];

tkEH[k_, cosmo_?OptionQ] := Module[{ag, rs, geff, ob, om, q, t0, l0, c0, g0}, 
	rs = rsEH[cosmo];
    ob = Omegabh2 /. cosmo;
    om = OmegaMh2 /. cosmo;
    ag = 1.0 - 0.328*Log[431*om] * (ob/om) + 0.38 * Log[22.3*om]*(ob/om)^2;
    g0 = om/Hubble[1.0, cosmo];
    geff = g0 * (ag + (1.0-ag)/(1.0 + (0.43*k*rs)^4));
    t0 = (TCMB/2.7) /. cosmo;
    q = k * t0^2 / geff;
    c0 = 14.2 + 731.0/(1+62.5*q);
    l0 = Log[2*Exp[1.0] + 1.8*q];
    l0/(l0 + c0*q^2)
    ];

pkEH[k_, ns_, deltah2_, cosmo_?OptionQ] := Module[{t1, Hbyc},
	Hbyc = 100.0/299792.458; 
	t1 = tkEH[k, cosmo];
	deltah2*(k/Hbyc)^ns * (1/Hbyc)^3 * t1^2 * 2*Pi^2 
];

End[];
EndPackage[];


