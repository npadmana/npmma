(* Mathematica Package *)

BeginPackage["Misc`MapProjections`"]
(* Exported symbols added here with SymbolName::usage *)  

(*--- Mollweide coordinates  -----

See http://mathworld.wolfram.com/MollweideProjection.html

Code copied from Mathematica Stack Exchange  http://mathematica.stackexchange.com/questions/13361/mollweide-maps-in-mathematica

We have removed the factor of Sqrt[2] from the definition given there, so x runs from -2 to 2, y runs from -1 to 1. 
longitude runs from -Pi to Pi, while latitude from -Pi/2 to Pi/2.
*)

mapMollweide::usage = "mapMollweide[{lon, lat}, lon0:0] maps lat, lon (in radians by default) to (x,y) in the Mollweide coordinate system. 
   Note that {theta, phi} are assumed to be in a list. The central longitude is assumed to be 0, unless specified otherwise";
mapInverseMollweide::usage = "mapInverseMollweide[{x, y}, lon0:0] maps Mollweide coordinates x,y to lon, lat (in radians). 
    x,y must be in a list. The central longitude is assumed to be zero";

Begin["`Private`"] (* Begin Private Context *) 

mapInverseMollweide[{x_, y_}, lon0_:0] := 
 With[{theta = ArcSin[y]}, {Pi x/(2 Cos[theta]) + lon0, 
   ArcSin[(2 theta + Sin[2 theta])/Pi]}];

mollweideHelper[phi_] := Module[{theta}, If[Abs[phi] == Pi/2, phi, theta /. 
 FindRoot[2 theta + Sin[2 theta] == Pi Sin[phi], {theta, phi}]]];
 
mapMollweide[{lon_, lat_}, lon0_:0] := With[{theta = mollweideHelper[lat]}, {2 /Pi*(lon-lon0) Cos[theta], Sin[theta]}]



End[] (* End Private Context *)
EndPackage[]