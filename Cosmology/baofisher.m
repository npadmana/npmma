BeginPackage["Cosmology`baofisher`"]
(* Translation of the code in Seo and Eisenstein 2007 *)

baoFisher::usage = "baoFisher[sigmaPerp_, sigmaPar_, nbar_, sigma8_, beta_, vol_, sigmaZ_:0.0] returns the fisher matrix"
baoError::usage = "baoError[sigmaPerp_, sigmaPar_, nbar_, sigma8_, beta_, vol_, sigmaZ_:0.0] returns the error as a 3-tuple"


Begin["`Private`"]

(* We define the many constants etc here to parallel the original C code *)
kstep = 0.01;
mustep0 = 0.05;
(* This is the power spectrum of WMAP-3,normalized to 1 at k=0.2 *)
pBAO = {14.10,20.19,16.17,11.49,8.853,7.641,6.631,5.352,4.146,3.384,3.028,2.799,2.479,2.082,1.749,
1.551,1.446,1.349,1.214,1.065,0.9455,0.8686,0.8163,0.7630,0.6995,0.6351,0.5821,0.5433,0.5120,0.4808,0.4477,
0.4156,0.3880,0.3655,0.3458,0.3267,0.3076,0.2896,0.2734,0.2593,0.2464,0.2342,0.2224,0.2112,0.2010,0.1916,0.1830,0.1748,0.1670,0.1596};
(* The power spectrum at k=0.2h Mpc^-1 for sigma8=1 *)
baoPower= 2710.0;
baoSilk = 8.38;
baoAmp = 0.05169;
kList = (Range[Length[pBAO]]-0.5)*kstep;
(* Precompute some numbers *)
silkList = Exp[-2.0*(kList*baoSilk)^1.4] * kList^2;

(* Code below here needs to be wrapped inside a module block *)
baoFisher[sigmaPerp_, sigmaPar_, nbar_, sigma8_, beta_, vol_, sigmaZ_:0.0] := Module [{sperp2, spar2, sz2, mustep, nP, mu2List, mutmp, sumList, matList, fisherMat}, 
  sperp2 = sigmaPerp^2;
  spar2 = sigmaPar^2;
  sz2 = sigmaZ^2;
  mustep = If[Sqrt[spar2 + sz2] > 3*sigmaPerp, mustep0/10.0, mustep0];
  nP = nbar * sigma8^2 * baoPower; 
  mu2List = Range[mustep/2.0, 1.0, mustep]^2;

  (* Define the function we integrate over mu, this is in itself an integral
    over k. Note that both integrals are done by simply summing. *)
    mutmp[mu2_] := Module[{tmpz, s2tot}, 
      tmpz = pBAO + Exp[kList^2 * sz2 * mu2]/nP/(1.0+ beta*mu2)^2;
      s2tot = sperp2*(1-mu2) + spar2*mu2;
      Total[silkList*Exp[-kList^2 * s2tot]/tmpz^2]
    ];
    sumList = mutmp /@ mu2List;
    matList = {{(1.0-#)^2, -(1.0-#)*#},{-(1.0-#)*#, #^2}}& /@ mu2List;
    Total[sumList*matList]*(baoAmp^2 * kstep * mustep* 10.0^13*vol)/8.0/Pi^2
];

baoError[sigmaPerp_, sigmaPar_, nbar_, sigma8_, beta_, vol_, sigmaZ_:0.0] :=Module[{mat},
  mat = Inverse[baoFisher[sigmaPerp, sigmaPar, nbar, sigma8, beta, vol, sigmaZ]];
  {Sqrt[mat[[1,1]]], Sqrt[mat[[2,2]]], mat[[1,2]]/Sqrt[mat[[1,1]] * mat[[2,2]]]}
]

End[]
EndPackage[]
