Package["GraphWalking`"]


PackageImport["GeneralUtilities`"]
PackageImport["HyperArrays`"]


(* ::Input:: *)
(*RandomWalkEnsemble[PathGraph[10],3,5]*)


(* ::Subsection:: *)
(*ArraySelectParity just picks cells of parity 0 or 1, or does nothing (All)*)


ArraySelectParity[array_, All] := array;
ArraySelectParity[array_, n_] := MapIndexed[With[{k = Mod[First[#2] + n, 2, 1]}, Part[#1, Span[k, k - 3, 2]]]&, array];


(* ::Subsection:: *)
(*EnsembleDensityPlot plots an ensemble, but applies various preprocessing before plotting.*)


(* ::Input:: *)
(*NormalDistribution*)


PackageExport["Decimation"]
PackageExport["CellRange"]
PackageExport["CellParity"]

Decimation::usage = "Decimation is an option that specifies how to decimate the history.";
CellRange::usage = "CellRange is the option that specifies the range of t and x values to plot.";
CellParity::usage = "CellParity is the option that specifies which parity (All, 0, or 1) to select.";

Options[EnsembleDensityPlot] = Join[
  DeleteCases[Options[ArrayPlot], DataRange -> _],
  {Decimation -> {}, Standardized -> True, CellRange -> All, CellParity -> All}
];


PackageExport["EnsembleDensityPlot"]

EnsembleDensityPlot[ensemble_, opts1___, CellParity -> "Seperate", opts2___] :=
  Column[Table[EnsembleDensityPlot[ensemble, CellParity -> p, PlotLabel -> Row[{"parity ", p}], opts1, opts2], {p, 0, 1}]];

EnsembleDensityPlot[ensemble_, opts:OptionsPattern[]] :=
  ArrayPlot[
    ArraySelectParity[ensemble, OptionValue[CellParity]] //
    ArrayTake[OptionValue[CellRange]] //
    ArrayDecimate[OptionValue[Decimation]] //
    If[OptionValue[Standardized], Map[Standardize], Identity] // Transpose,
    FilterOptions[opts], Frame -> False,
    PlotRangePadding -> 0, ColorFunction -> "Rainbow",PixelConstrained -> 8
  ]


PackageExport["RandomWalkEnsemblePlot"]

RandomWalkEnsemblePlot[args___, opts___Rule] := EnsembleDensityPlot[RandomWalkEnsemble[args], opts];