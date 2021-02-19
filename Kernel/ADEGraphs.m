Package["GraphWalking`"]


PackageImport["GeneralUtilities`"]

PackageExport["$A"]
PackageExport["$D"]
PackageExport["$E6"]
PackageExport["$E7"]
PackageExport["$E8"]

PackageExport["$EGraphs"]

Unprotect[PathGraph];
(* fix a weird oversight in the design of PathGraph *)
PathGraph[n_Integer, opts___] := PathGraph[Range[n], opts];


$A[n_] := PathGraph[Range[n], VertexCoordinates -> Thread[{Range[n], 0}]];
$D[n_] := EdgeAdd[PathGraph[n], 0 <-> 2, VertexCoordinates -> Append[Thread[{Range[n], 0}], {2, 1}]];
$$E[n_] := EdgeAdd[PathGraph[n], 0 <-> 3, VertexCoordinates -> Append[Thread[{Range[n], 0}], {3, 1}]]
$E6 = $$E[5];
$E7 = $$E[6];
$E8 = $$E[7];
$EGraphs = {$E6, $E7, $E8};

getVertexCoordinates[graph_] := Options[graph, VertexCoordinates][[1,2]];
addAffineVertex[graph_, vertex_, offset_] := Scope[
  vcoords = getVertexCoordinates[graph];
  graph = EdgeAdd[graph, Thread[-1 <-> vertex]];
  Graph[graph, VertexCoordinates -> Append[vcoords, vcoords[[VertexIndex[graph, First[vertex, vertex]]]] + offset]]
];


PackageExport["$ATilde"]
PackageExport["$DTilde"]
PackageExport["$E6Tilde"]
PackageExport["$E7Tilde"]
PackageExport["$E8Tilde"]
PackageExport["$D4Tilde"]

PackageExport["$ETildeGraphs"]

$ATilde[n_] := addAffineVertex[$A[n], {1, n}, {0, 1}];
$DTilde[n_] := addAffineVertex[$D[n], (n-1), {0, 1}];
$E6Tilde = addAffineVertex[$E6, 0, {0, 1}];
$E7Tilde = addAffineVertex[$E7, 1, {-1, 0}];
$E8Tilde = addAffineVertex[$E8, 7, {1, 0}];
$D4Tilde = addAffineVertex[$D[3], 2];

$ETildeGraphs = {$E6Tilde, $E7Tilde, $E8Tilde};



PackageExport["MutationGameStateGraph"]


PackageExport["PopulationRoot"]

fmtNum[i_Integer] := If[Negative[i], OverBar[Abs[i]], i];

Format[PopulationRoot[vec_List]] := Row[fmtNum /@ vec];

mutate[population_, i_, neighbors_] :=
  MapAt[-# + Total[population[[neighbors]]]&, population, i];

ms_mutationSuccessors[PopulationRoot[population_]] := PopulationRoot /@ ms[population]

mutationSuccessors[neighborList_List][population_List] :=
  Table[mutate[population, i, neighborList[[i]]], {i, Length[population]}]

graphToNeighborList[graph_] := VertexOutComponent[graph, #, {1}]& /@ VertexList[graph];

MutationGameStateGraph[graph_Graph, n_, offset_:0] := Scope[
  nbors = graphToNeighborList[IndexGraph[graph]];
  succs = mutationSuccessors[nbors];
  count = VertexCount[graph]; vertices = VertexList[graph];
  id = IdentityMatrix[count] + offset;
  init = PopulationRoot /@ Join[id, -id];
  formatGraph @ SimpleGraph @ ExploreGraph[succs, init, "MaxDepth" -> n, DirectedEdges -> False]
];

formatGraph[e_] := Graph[e,
  EdgeShapeFunction -> Function[Line[#1]], VertexShapeFunction -> Function[Point[#]],
  VertexStyle -> Directive[GrayLevel[0, .4], AbsolutePointSize[5]],
  EdgeStyle -> Directive[Thickness[Medium], GrayLevel[0,.2]]
];



