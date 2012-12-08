(* Mathematica Package *)

BeginPackage["Misc`npPlotting`"]
(* Exported symbols added here with SymbolName::usage *) 

npSquare::usage="npSquare is a square";
npDisk::usage="npDisk is a filled circle";
npLogTicks::usage="npLogTicks[logstart, logend, bigtickfunc] generates a list of ticks that I like for plotting log plots.

logstart and logend are the base 10 logarithms of the start and end functions. bigtickfunc is a function that gets the 
exponent, and formats it accordingly. It defaults to Superscript[10,#]& .

Note that this is not meant to be fully general, just the most common case.
";

 

Begin["`Private`"] (* Begin Private Context *) 

npSquare = Graphics[{Rectangle[]}];
npDisk = Graphics[{Disk[]}];

bigtickfuncDefault[i_] = Superscript[10,i];
npLogTicks[logstart_, logend_, bigtickfunc_:bigtickfuncDefault] :=
    Module[ {bigticks, smallticks},
        smallticks = 
         Table[{10.0^i*j, "", {0.01, 0}}, {i, logstart, logend-1}, {j, 2., 
            9}] // Flatten[#, {{1, 2}, {3}}] &;
        bigticks = 
         Table[{10.0^i, bigtickfunc[i], {0.02, 0}}, {i, 
           logstart, logend}];
        Join[bigticks, smallticks]
    ];



End[] (* End Private Context *)

EndPackage[]