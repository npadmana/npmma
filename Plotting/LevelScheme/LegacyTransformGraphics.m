(* ::Package:: *)

(************************************************************************)
(* This file was generated automatically by the Mathematica front end.  *)
(* It contains Initialization cells from a Notebook file, which         *)
(* typically will have the same name as this file except ending in      *)
(* ".nb" instead of ".m".                                               *)
(*                                                                      *)
(* This file is intended to be loaded into the Mathematica kernel using *)
(* the package loading commands Get or Needs.  Doing so is equivalent   *)
(* to using the Evaluate Initialization Cells menu command in the front *)
(* end.                                                                 *)
(*                                                                      *)
(* DO NOT EDIT THIS FILE.  This entire file is regenerated              *)
(* automatically each time the parent Notebook file is saved in the     *)
(* Mathematica front end.  Any changes you make to this file will be    *)
(* overwritten.                                                         *)
(************************************************************************)



(* LegacyTransformGraphics -- Modification of TransformGraphics, extracted from Graphics`Graphics` Legacy Standard Add-On Package for Mathematica 6.0 (MathSource 6868)

Modified by Mark A. Caprio, August 2007:
  -- to load as top-level context (to work better with Needs)
  -- without obsolete package warning message 
  -- supports Point with multiple coordinate arguments
September 2008
  -- Mathematica 6 compatibility fix for TransformGraphics on density and contour graphics: do not strip options on GraphicsComplex
      (courtesy of Robert Collyer, LSU)
February 2009
  -- Remove all public functions other than TransformGraphics, to avoid name clash with new Mathematica 7 kernel plotting functions
September 2009
  -- Permit handling of GraphicsComplex containing non-indexed points
July 2010
  -- Permit handling of graphics within Style[]

Context changed to LevelScheme`*.

*)

(* :Name: Graphics`Graphics` *)

(* :Title: Additional Graphics Functions *)

(* :Author: Wolfram Research, Inc. *)

(* :Copyright: Copyright 1990-2007, Wolfram Research, Inc.*)

(* :Mathematica Version: 5.0 *)

(* :Package Version: 2.0 *)

(* :History:
   Original Version by Wolfram Research, Inc.
   Revised by Michael Chan and Kevin McIsaac (Wolfram Research), March 1990.  
   Further revisions by Bruce Sawhill (Wolfram Research), September 1990.
   Further revisions by E.C. Martin (Wolfram Research), December 1990.
   Removal of 3D graphics functions to the package Graphics3D.m and
        minor revisions by John M. Novak, March 1991.
   More extensive revisions by John M. Novak, November 1991.
        (PieChart, log plots, ScaledPlot, bar charts, etc.)
   Some significant Log plot bug fixes by John M. Novak, October 1994.
   More Log plot bug fixes by John M. Novak, May 1995.
   Rename Scale option to ScaleFunction to avoid name collision with
         another package, June 1995.
   Histogram, ECM, October 1997.
   Revise DisplayTogether, John M. Novak, January 2000.
*)

(*:Summary:
This package provides special functions for plotting in two
dimensions.  Special formats include bar charts, pie charts,
log plots, polar plots, error bar plots, and histograms.
*)

(* :Context: Graphics`Graphics` *)

(*:Keywords:
    Log, Graphics, ListPlot, Scale, Polar, histogram
*)

(*:Requirements: None. *)

(*:Warnings:
    Expands the definitions of PlotStyle.
*)

(*:Sources: *)

(*Message[General::obspkg, "Graphics`Graphics`"]*)
Quiet[
BeginPackage["LevelScheme`LegacyTransformGraphics`",
   {"Graphics`Common`GraphicsCommon`","Utilities`FilterOptions`", 
    "Statistics`DataManipulation`"}]
, {General::obspkg, General::newpkg}];


(* Usage messages *)

TransformGraphics::usage =
"TransformGraphics[expr, f] applies the function f to all \
coordinates of graphics primitives in expr.";

SkewGraphics::usage =
"SkewGraphics[graphics, m] applies the matrix m to all coordinates in graphics.";


Begin["`Private`"];

issueObsoleteFunMessage[fun_, context_] := Message[General::obspkgfn, fun, context];

(* Define a better NumberQ *)

numberQ[x_] := NumberQ[N[x]];




(* TransformGraphics *)

TransformGraphics[HoldPattern[Graphics][list_, opts___], f_, p_:0] :=
    (issueObsoleteFunMessage[TransformGraphics,"Graphics`Graphics`"];
	Graphics[ TG0[list, f, p], opts ])

TG0[d_List, f_, p_:0] := Map[ TG0[#, f, p]& , d ]

(* mc: original *)
(*TG0[HoldPattern[GraphicsComplex][pts_, prims_, o___], f_, p_:0] :=
    GraphicsComplex[If[VectorQ[pts], pts, Map[f, pts]],
         TG0[prims, f, Length[pts]]]*)
(* mc: not stripping options *)
TG0[HoldPattern[GraphicsComplex][pts_, prims_, o___], f_, p_:0] :=
    GraphicsComplex[If[VectorQ[pts], pts, Map[f, pts]],
         TG0[prims, f, Length[pts]],o]
(* mc: normalizing graphics complex -- fails since Normal seems to be incompletely implemented (Mathematica 7) *)
(*TG0[HoldPattern[GraphicsComplex][pts_, prims_, o___], f_, p_:0] :=
    TG0[Normal[GraphicsComplex[pts,prims,o]], f,p];*)

(* mc: original *)
(* TG0[Point[d_], f_, p_:0]/;(p === 0 || !gcq[d, p]) := Point[f[d]]*)
(* mc: allowing for collection of points *)
TG0[Point[d_?VectorQ], f_, p_:0]/;(p === 0 || !gcq[d, p]) :=Point[f[d]];
TG0[Point[d_?MatrixQ], f_, p_:0]/;(p === 0 || !gcq[d, p]) :=Point[f/@d];

TG0[Line[d_List], f_, p_:0]/;(p === 0 || !gcq[d, p]) := Line[Map[f, d, {-2}]]
(* mc: note mapping at level -2 causes f to act on {dx,dy} of Offset coordinate, perhaps not desirable *)

TG0[Arrow[d_List, o___], f_, p_:0]/;(p === 0 || !gcq[d, p]) :=
       Arrow[Map[f, d, {-2}], o]

TG0[Rectangle[{xmin_, ymin_}, {xmax_, ymax_}], f_, p_:0] :=
    TG0[Polygon[{{xmin,ymin}, {xmin,ymax}, {xmax, ymax}, {xmax, ymin}}], f, p]

TG0[Polygon[d_List], f_, p_:0]/;(p === 0 || !gcq[d, p]) := Polygon[Map[f, d, {-2}]]

TG0[Circle[d_List, r_?numberQ, t___], f_, p_:0] :=
    Circle[f[{x}], f[{r,r}], t]

TG0[Circle[d_List, r_List, t___], f_, p_:0] :=
        Circle[f[d], f[r], t] 

TG0[Disk[d_List, r_?numberQ, t___], f_, p_:0] :=
        Disk[f[d], f[{r,r}], t] 

TG0[Disk[d_List, r_List, t___], f_, p_:0] := 
        Disk[f[d], f[r], t]

TG0[Raster[array_, range:{{_,_},{_,_}}:{{0,0}, {1,1}}, zrange___], f_, p_:0] := 
    Raster[array, f /@ range, zrange]

TG0[RasterArray[array_, range:{{_,_},{_,_}}:{{0,0}, {1,1}}, zrange___], f_, p_:0] := 
    RasterArray[array, f /@ range, zrange]

TG0[Text[expr_, d_List, opts___], f_, p_:0] := Text[expr, f[d], opts]


(* mc: to handle graphics within Style *)
TG0[Style[expr_,opts___],f_, p_:0] :=
    Style[TG0[expr, f, p],opts];

TG0[expr_, f_, p_:0] := expr


(* gcq tests for compliance with a GraphicsComplex indexed primitive *)
gcq[_Integer] := True
gcq[pts:{__Integer}, count_]/;(Max[pts] <= count && Min[pts] >= 1) := True
gcq[pts:{{_Integer..}..}, count_]/;(Max[pts] <= count && Min[pts] >= 1) := True
gcq[any_] := False

(* mc: to properly handle non-indexed primative inside a GraphicsComplex *)
gcq[any_,count_] := False;

(* SkewGraphics *)

SkewGraphics[g_, m_?MatrixQ] :=
    (issueObsoleteFunMessage[SkewGraphics,"Graphics`Graphics`"];
	TransformGraphics[g, (m . #)&])


End[ ];   (* Graphics`Graphics`Private` *)

EndPackage[ ];  (* Graphics`Graphics` *)