(* ::Package:: *)

(* ::Title:: *)
(*Plotting functions*)


(* ::Text:: *)
(*Some functions for consistent custom plots across notebooks.*)


<<PlotLegends`


(* ::Section:: *)
(*Standard options*)


plotStyles={Black,{Black,Dotted},{Black,Dashed},{Black,DotDashed},Black,Black};
plotMarkers={"","","","",Graphics[GraphicsComplex[Flatten[{{{0,0}},Table[15{Cos[t],Sin[t]},{t,2Pi/8,2Pi,2Pi/4}]},1],{Line[{2,3,4,5,2}]}],ImageSize->6],Graphics[{Black,Disk[]},ImageSize->6]};
plotOptions={ImageSize->400,Joined->True,PlotRange->{Full,Full},GridLines->Automatic,GridLinesStyle->None,AxesLabel->{"N","\!\(\*SubscriptBox[\(N\), \(s\)]\)"},PlotStyle->plotStyles,PlotMarkers->plotMarkers};
histogramOptions={PlotRange->Full,ImageSize->400,GridLines->Automatic,ChartStyle->Gray};


LegendFormat[channelType_]:=If[ListQ[channelType],If[Length[channelType]==2,channelType[[1]]<>"-"<>ToString[channelType[[2]]],channelType[[1]]],channelType]


legendOptions={LegendPosition->{0.1,0},LegendSize->{0.7,0.5},LegendShadow->None}


(* ::Section:: *)
(*Error histogram*)


CustomHistogram[x_,label_]:=Histogram[Flatten[x],Automatic,"PDF",histogramOptions,AxesLabel->label]


(* ::Section:: *)
(*Statistics table*)


StatsTable[x_]:=TableForm[{{Mean[x//Flatten],StandardDeviation[x//Flatten]}},TableHeadings->{{},{"\!\(\*StyleBox[\"\[Mu]\", \"TR\"]\)","\[Sigma]"}}]


(* ::Section:: *)
(*Error table*)


MaxErrorsTable[data_,n_]:=Module[{maxErrors,p},
	maxErrors = Sort[Flatten[data],Greater][[1;;n]];
	p = Flatten[Table[Position[data,maxErrors[[i]]],{i,Length[maxErrors]}]];
	TableForm[Flatten[{{{"\[Gamma] (dB)","n","m","\[Epsilon]","\!\(\*SubscriptBox[\(\[Epsilon]\), \(r\)]\)"}},Table[{10Log[10,y[[p[[i]]]]]//Round,x[[p[[i]]]],If[mRange//ListQ,w[[p[[i]]]],1],Extract[error,p[[i]]],Extract[relError,p[[i]]]}//N,{i,Length[p]}]},1]]
]
