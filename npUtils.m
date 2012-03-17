BeginPackage["npUtils`"]

deg2tostr::usage = "Convert sky area in deg^2 to steradians"

Begin["`Private`"]

deg2tostr[area_] := area * (Pi/180.0)^2;

End[]
EndPackage[]

