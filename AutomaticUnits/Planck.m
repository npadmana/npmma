UnitSet["Planck"]={
	DeclareUnit["PlanckLength",Unit[1.61625281*^-35,"Meter"],
		UsageMessage->"PlanckLength is the Planck unit of distance.",TraditionalLabel->SubscriptBox["\[ScriptL]","P"]],
	DeclareUnit["PlanckMass",Unit[2.1764411*^-8,"Kilogram"],
		UsageMessage->"PlanckMass is the Planck unit of mass.",TraditionalLabel->SubscriptBox["m","P"]],
	DeclareUnit["PlanckTime", Unit[5.3912427*^-44 ,"Second"],
		UsageMessage->"PlanckTime is the Planck unit of time.",TraditionalLabel->SubscriptBox["t","P"]],
	DeclareUnit["PlanckTemperature",Unit[1.41678571*^32,"Kelvin"],		
		UsageMessage->"PlanckTemperature is the Planck unit of temperature.",TraditionalLabel->SubscriptBox["T","P"]],
	DeclareUnit["PlanckCharge",Unit[1.8755459*^-18,"Coulomb"],
		UsageMessage->"PlanckCharge is the Planck unit of charge.",TraditionalLabel->SubscriptBox["q","P"]],
(*Derived units*)
	DeclareUnit["PlanckArea",2.61223*^-70 Unit["Meter"]^2,
		UsageMessage->"PlanckArea is the Planck unit of area.",TraditionalLabel->SubscriptBox[SuperscriptBox["\[ScriptL]","2"],"P"]],
	DeclareUnit["PlanckVolume",4.22419*^-105 Unit["Meter"]^3,
		UsageMessage->"PlanckVolume is the Planck unit of volume.",TraditionalLabel->SubscriptBox[SuperscriptBox["\[ScriptL]","3"],"P"]],
	DeclareUnit["PlanckMomentum",6.52485 Unit["Kilogram"]Unit["Meter"]/Unit["Second"],
		UsageMessage->"PlanckMomentum is the Planck unit of momentum.",TraditionalLabel->RowBox[{SubscriptBox["m","P"],"c"}]],
	DeclareUnit["PlanckEnergy",Unit[1.9561*^9,"Joule"],
		UsageMessage->"PlanckEnergy is the Planck unit of energy.",TraditionalLabel->SubscriptBox["E","P"]],
	DeclareUnit["PlanckForce",Unit[1.21027*^44,"Newton"],
		UsageMessage->"PlanckForce is the Planck unit of force.",TraditionalLabel->SubscriptBox["F","P"]],
	DeclareUnit["PlanckPower",Unit[3.62831*^52,"Watt"],
		UsageMessage->"PlanckPower is the Planck unit of power.",TraditionalLabel->SubscriptBox["P","P"]],
	DeclareUnit["PlanckDensity",5.15500*^96 Unit["Kilogram"]/Unit["Meter"]^3,
		UsageMessage->"PlanckDensity is the Planck unit of density.",TraditionalLabel->SubscriptBox["\[Rho]","P"]],
	DeclareUnit["PlanckAngularFrequency",1.85487*^43/Unit["Second"],
		UsageMessage->"PlanckAngularFrequency is the Planck unit of frequency.",
		TraditionalLabel->SubscriptBox["\[Omega]","P"]],
	DeclareUnit["PlanckPressure",Unit[4.63309*^113,"Pascal"],
		UsageMessage->"PlanckPressure is the Planck unit of pressure.",TraditionalLabel->SubscriptBox["p","P"]],
	DeclareUnit["PlanckCurrent",Unit[3.4789*^25,"Ampere"],
		UsageMessage->"PlanckCurrent is the Planck unit of current.",TraditionalLabel->SubscriptBox["I","P"]],
	DeclareUnit["PlanckVoltage",Unit[1.04295*^27,"Volt"],
		UsageMessage->"PlanckVoltage is the Planck unit of voltage.",TraditionalLabel->SubscriptBox["V","P"]],
	DeclareUnit["PlanckImpedence",Unit[29.9792458,"Ohm"],
		UsageMessage->"PlanckImpedence is the Planck unit of impedence.",TraditionalLabel->SubscriptBox["Z","P"]]
};