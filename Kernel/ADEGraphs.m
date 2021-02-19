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



Clear[MutationGameStateGraph];
PackageExport["MutationGameStateGraph"]
PackageExport["PopulationRoot"]

fmtNum[i_Integer] := If[Negative[i], OverBar[Abs[i]], i];

Format[PopulationRoot[vec_List]] := Row[fmtNum /@ vec];

mutate[population_, i_, neighbors_] :=
  MapAt[-# + Total[population[[neighbors]]]&, population, i];

ms_mutationSuccessors[PopulationRoot[population_]] := PopulationRoot /@ ms[population]

toModulusFunction = MatchValues[
  None|Infinity := Identity;
  n_Integer := Function[Mod[#, n]];
  n:{(_Integer|Infinity|None)..} := ReplaceAll[Function[Mod[#, n]], None -> Infinity];
];

mutationSuccessors[neighborList_List, modFunc_][population_List] :=
  Table[modFunc @ mutate[population, i, neighborList[[i]]], {i, Length[population]}]

PackageExport["VertexNeighborList"]

repeat[i_, 1] := i;
repeat[i_, n_] := Splice[ConstantArray[i, n]];
VertexNeighborList[graph_Graph] := Scope[
  verts = VertexList[graph];
  Function[row,
    repeat[Part[verts, First @ #], #2]& @@@ Most[ArrayRules @ row]
  ] /@ AdjacencyMatrix[graph]
];

Options[MutationGameStateGraph] = {
  Modulus -> None,
  MaxVertices -> 1000,
  MaxDepth -> Infinity,
  MaxEdges -> Infinity
};

MutationGameStateGraph::maxverts = "MaxVertices value of `` was reached, graph is not complete."

MutationGameStateGraph[graph_Graph, init:Except[_Rule], steps_Integer, opts:OptionsPattern[]] :=
  MutationGameStateGraph[graph, init, MaxDepth -> steps, opts];

MutationGameStateGraph::badpoproots = "PopulationRoots in init did not all have dimensions of ``."
MutationGameStateGraph::badinit = "Unrecognized initial condition."

MutationGameStateGraph[graph_Graph, init:Except[_Rule]:Automatic, OptionsPattern[]] := Scope[
  nbors = VertexNeighborList[IndexGraph[graph]];
  UnpackOptions[modulus, maxVertices, maxDepth, maxEdges];
  succs = mutationSuccessors[nbors, toModulusFunction @ modulus];
  count = VertexCount[graph]; vertices = VertexList[graph];
  Which[
    NumberQ[init] || init === Automatic,
      id = IdentityMatrix[count] + Replace[init, Automatic -> 0];
      init = PopulationRoot /@ Join[id, -id],
    VectorQ[init, IntegerQ] && Length[init] === count,
      init = {PopulationRoot[init]},
    MatrixQ[init, IntegerQ] && Length[First[init]] === count,
      init = PopulationRoot /@ init,
    MatchQ[init, {__PopulationRoot} | _PopulationRoot],
      init = Developer`ToList[init];
      zeros = ConstantArray[0, count];
      init = init /. PopulationRoot[r:({__Rule} | _Rule)] :> PopulationRoot[ReplacePart[zeros, r]];
      If[!MatchQ[init, {Repeated @ PopulationRoot[{Repeated[_Integer, count]}]}],
        Message[MutationGameStateGraph::badpoproots, count];
        Return[$Failed]];
    ,
    True,
      Message[MutationGameStateGraph::badinit];
      Return[$Failed]
  ];
  result = ExploreGraph[succs, init, MaxVertices -> maxVertices, MaxDepth -> maxDepth, MaxEdges -> maxEdges, DirectedEdges -> False,
    "TerminationReasonFunction" -> Function[If[# === "MaxVerticesReached", Message[MutationGameStateGraph::maxverts, maxVertices]]]
  ];
  formatGraph @ SimpleGraph @ result
];

formatGraph[e_] := Graph[e,
  EdgeShapeFunction -> Function[Line[#1]], VertexShapeFunction -> Function[Tooltip[Point[#], #2]],
  VertexStyle -> Directive[GrayLevel[0, .4], AbsolutePointSize[3]],
  EdgeStyle -> Directive[Thickness[Medium], GrayLevel[0,.2]]
];


PackageExport["VertexContractBy"]

VertexContractBy[graph_Graph, func_] :=
  VertexContract[graph, GatherBy[VertexList[graph], func]]


PackageExport["PopulationRootGallery"]

Clear[PopulationRootGallery];

Options[PopulationRootGallery] = {
  MaxDepth -> 4, MaxVertices -> 500, Modulus -> None
}

PopulationRootGallery[mutationGraph_Graph, maxPop_Integer, opts:OptionsPattern[]] := Scope[
  inits = PopulationRoot /@ Tuples[Range[-maxPop, maxPop], VertexCount[mutationGraph]];
  UnpackOptions[maxDepth, maxVertices];
  gameOpts = Sequence[MaxDepth -> maxDepth, MaxVertices -> maxVertices];
  stateGraphs = {}; vertexIndex = <||>;
  Do[
    newStateGraph = Quiet @ MutationGameStateGraph[mutationGraph, init, gameOpts];
    newVertexList = VertexList[newStateGraph];
    overlappingGraphIndices = DeleteDuplicates @ DeleteMissing @ Lookup[vertexIndex, newVertexList];
    If[overlappingGraphIndices =!= {},
      previousGraphs = Part[stateGraphs, overlappingGraphIndices];
      Part[stateGraphs, overlappingGraphIndices] = None;
      (*
      sharedVertices = Union @@ (Intersection[newVertexList, VertexList[#]]& /@ previousGraphs);
      overlapGraph = MutationGameStateGraph[mutationGraph, sharedVertices, MaxDepth -> 1, MaxVertices -> maxVertices];
      newStateGraph = GraphUnion @@ Join[previousGraphs, {newStateGraph, overlapGraph}];
      *)
      newStateGraph = formatGraph[GraphUnion @@ Append[previousGraphs, newStateGraph]];
    ];
    If[!GraphQ[newStateGraph], Return[$Failed]];
    AppendTo[stateGraphs, newStateGraph];
    AppendTo[vertexIndex, Thread[VertexList[newStateGraph] -> Length[stateGraphs]]];
  ,
    {init, inits}
  ];
  stateGraphs = DeleteNone[stateGraphs];
  DeleteDuplicates[stateGraphs, IsomorphicGraphQ]
]