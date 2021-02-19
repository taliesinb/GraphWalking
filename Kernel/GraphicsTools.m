Package["GraphWalking`"]


PackageImport["GeneralUtilities`"]
PackageImport["HyperArrays`"]



PackageExport["ComplexDisk"]

ComplexDisk[c_, sz_:20, padding_:3] := With[{color = Hue[Arg[c]/(2 Pi)+.05, Min[Sqrt[Abs[c]]/2,1]]},
  Graphics[
    {color, EdgeForm[Darker[color, .2]], Disk[{0,0},1]},
    ImageSize -> {sz, sz}, PlotRange -> {{-1,1},{-1,1}},
    ImagePadding -> padding, Frame -> False,
    Axes -> None, FrameTicks -> None
  ]
];


PackageExport["ComplexArrow"]

ComplexArrow[num_, sz_:30] := Scope[
  c = Chop[num];
  mag = Abs[c];
  col = Darker @ Which[mag > 8, Blue, mag > 4, Green, mag > 2, Red, mag > 1, Orange, mag == 1, Black, mag > .1, Gray, True, LightGray];
  If[mag > 1, c = c / mag; shrink = Sqrt[mag], shrink = 1];
  {re, im} = ReIm[c];
  pnt = {re, im};
  g = Graphics[{
    {EdgeForm[LightGray], FaceForm[White], Disk[Offset[{0, 0}, {0, 0}], 1 / shrink]},
    If[mag > 0, {col, Line[{Offset[{0, 0}, {0, 0}], Offset[{0, 0}, pnt]}], AbsolutePointSize[3], Point[pnt]}]},
    ImageSize -> {sz, sz}, PlotRange -> {{-1, 1}, {-1, 1}}, ImagePadding -> 3, Frame -> False, Axes -> None, FrameTicks -> None];
  g
];


PackageExport["ComplexPanel"]
PackageExport["ComplexGraphicalRow"]

ComplexGraphicalRow[c_, sz_] := Row[{ComplexArrow[c, sz], ComplexDisk[c, sz]}];
ComplexPanel[c_] := PrettyGrid[
  <|"Degrees" -> Round[N[Arg[c]] / Degree],
    "Radians" -> Arg[c] /. Rational[a_, b_] :> Row[{a, "/", b}],
    "Magnitude" -> Round[Abs[c], .01], "Re" -> Re[c], "Im" -> Im[c],
    "Visual" -> ComplexGraphicalRow[c, 20]
  |>
];


PackageExport["RootsOfUnity"]

RootsOfUnity[nthroot_Integer?Positive] := ExpToTrig @ Table[Exp[2 Pi I i/nthroot], {i, 0, nthroot - 1}]