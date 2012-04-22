UnitSet["Astronomical"]={
	(*Distance*)
	DeclareUnit["LightYear", Unit[9460730472580800,"Meter"], UsageMessage->"LightYear is a unit of length.",TraditionalLabel -> "ly"], 
	DeclareUnit["AstronomicalUnit", Unit[1.49597870691`12*^11, "Meter"], 
                    UsageMessage->"AstronomicalUnit is a unit of length.",TraditionalLabel -> "AU"],
	DeclareUnit["AU", Unit[1, "AstronomicalUnit"], UsageMessage->"AU is a unit of length.",TraditionalLabel -> "AU"], 
        DeclareUnit["Parsec", Unit[Cot[Pi/(3600*180)], "AstronomicalUnit"], UsageMessage->"Parsec is a unit of length.",TraditionalLabel -> "pc"],
	DeclareUnit["Mpc", Unit[10^6, "Parsec"], UsageMessage->"Mpc is Megaparsecs.",TraditionalLabel -> "Mpc"],
	DeclareUnit["LunarDistance", Unit[384403000, "Meter"], UsageMessage->"LunarDistance is a unit of length."], 
	DeclareUnit["EarthRadius", Unit[6371000, "Meter"], UsageMessage->"EarthRadius is a unit of length.",TraditionalLabel -> SubscriptBox["R","\[CirclePlus]"]], 
	(*Mass*)
	DeclareUnit["SolarMass", Unit[1.998922*^33,"Gram"], UsageMessage->"SolarMass is a unit of mass."], (*http://en.wikipedia.org/wiki/Solar_Mass*)
	DeclareUnit["EarthMass", Unit[5.9742*10^24, "Kilogram"], UsageMessage->"EarthMass is a unit of mass."], 	
	DeclareUnit["JupiterMass", Unit[1.8986*10^27, "Kilogram"], UsageMessage->"JupiterMass is a unit of mass."],
	(*Time*)
	Unit["Day"],
	DeclareUnit["JulianYear",Unit[365.25,"Day"],UsageMessage->"JulianYear is a unit of time."],
	DeclareUnit["JulianCentury",Unit[100,"JulianYear"],UsageMessage->"JulianCentury is a unit of time.",TraditionalLabel -> "D"]
};

UnitSet["Hubble"]={
        (*Other*)
        DeclareUnit["HubbleConstant",Unit[100, "Kilometer"/"Second"/"Mpc"],
                    UsageMessage->"The Hubble Constant"],
        DeclareUnit["HubbleTime", Unit[1, 1/"HubbleConstant"], UsageMessage->"HubbleTime is 1/H0."],
        DeclareUnit["SpeedOfLight",Unit[299792458,"Meter"/"Second"], UsageMessage->"SpeedOfLight is the speed of light in m/s."],
        DeclareUnit["HubbleDistance",Unit[1,"SpeedOfLight" "HubbleTime"], UsageMessage->"HubbleDistance is c/H0."]
};
