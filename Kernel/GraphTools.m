Package["GraphWalking`"]


PackageImport["GeneralUtilities`"]
PackageImport["HyperArrays`"]


PackageExport["GraphRelabel"]

GraphRelabel[graph_Graph, f_] := VertexReplace[graph, Map[# -> f[#]&, VertexList[graph]]];


PackageExport["VertexContractExplicit"]

VertexContractExplicit[g_, vertices_] :=
  VertexReplace[
    VertexContract[g, vertices],
    First[vertices] -> ContractedVertex[vertices]
  ];

PackageExport["ContractedVertex"]

Format[ContractedVertex[v_]] := CirclePlus @@ v;


PackageExport["GraphDisjointUnionExplicit"]

relabelSubGraph[graph_, {i_}] := GraphRelabel[graph, SubGraph[i]];
GraphDisjointUnionExplicit[graphs__Graph, opts:OptionsPattern[Graph]] :=
  GraphUnion[Sequence @@ MapIndexed[relabelSubGraph, {graphs}], opts];

PackageExport["SubGraph"]

SubGraph[i_][e_] := SubGraph[e, i];
Format[SubGraph[a_, i_]] := Subscript[a, i];


PackageExport["GraphHingeUnion"]

relabelWithHinge[graph_ -> joinVertex_, {i_}] :=
  relabelSubGraph[graph, {i}] -> SubGraph[joinVertex, i];

relabelWithHinge[graph_Graph, {i_}] :=
  relabelSubGraph[graph, {i}] -> SubGraph[First @ VertexList[graph], i];

GraphHingeUnion[graphs__Graph, opts___Rule] := GraphHingeUnion[{graphs}, opts];

GraphHingeUnion[graphRules_List, opts___] := Scope[
  {graphs, vertices} = KeysValues @ MapIndexed[relabelWithHinge, graphRules];
  result = VertexContractExplicit[GraphUnion @@ graphs, vertices];
  Graph[result, opts]
];


PackageExport["RestrictedGraphPower"]

ProductVertex::usage = "ProductState[a, b] represents the first particle being in state a, and second in state b.";
Format[ProductVertex[a_, b_], StandardForm] := CircleTimes[a, b];
Format[ProductVertex[a_, b_], TraditionalForm] := CircleTimes[a, b];
procEdge[a_ \[UndirectedEdge] b_] := KeyAppendTo[$undirectedSuccessors, a, b];
procEdge[a_ \[DirectedEdge] b_] := KeyAppendTo[$directedSuccessors, a, b];

makeBiedges[p:ProductVertex[a_, b_]] := {
  p \[DirectedEdge] ProductVertex[a, #]& /@ DeleteCases[Lookup[$directedSuccessors, Key @ b, {}], a],
  p \[DirectedEdge] ProductVertex[#, b]& /@ DeleteCases[Lookup[$directedSuccessors, Key @ a, {}], b],
  p \[UndirectedEdge] ProductVertex[a, #]& /@ DeleteCases[Lookup[$undirectedSuccessors, Key @ b, {}], a],
  p \[UndirectedEdge] ProductVertex[#, b]& /@ DeleteCases[Lookup[$undirectedSuccessors, Key @ a, {}], b]
}

RestrictedGraphPower[g_, 2, opts:OptionsPattern[Graph]] := Scope[
  $directedSuccessors = $undirectedSuccessors = <||>;
  Scan[procEdge, EdgeList[g]];
  vertices = VertexList[g];
  bivertices = ProductVertex @@@ Discard[Tuples[vertices, {2}], Apply[Equal]];
  biedges = Flatten @ Map[makeBiedges, bivertices];
  Graph[bivertices, biedges, opts]
]


PackageExport["TransformGraphCoordinates"]

TransformGraphCoordinates[f_, graph_, method_] :=
  Graph[graph, VertexCoordinates -> Map[f, GraphEmbedding[graph, method]]];


PackageExport["RadialClockGraph"]

radialPred[n_][ProductVertex[n1_, m1_], ProductVertex[n2_, m2_]] := Mod[n1 + 1, n, 1] == n2 && Abs[m1 - m2] <= 1;
radialClockCoord[ntotal_][ProductVertex[n_, m_]] := AngleVector[{1+m, 2Pi * n / ntotal}];
RadialClockGraph[n_Integer, m_Integer, opts:OptionsPattern[Graph]] := Scope[
  vertices = ProductVertex @@@ Tuples[{Range[n], Range[m]}];
  RelationGraph[radialPred[n], vertices,
    VertexCoordinates -> Map[radialClockCoord[n], vertices], opts]
]


PackageExport["RadialClockGraphStrict"]

radialPredStrict[n_][ProductVertex[n1_, m1_], ProductVertex[n2_, m2_]] := Mod[n1 + 1, n, 1] == n2 && Abs[m1 - m2] == 1;
RadialClockGraphStrict[n_Integer, m_Integer, opts:OptionsPattern[Graph]] := Scope[
  vertices = ProductVertex @@@ Tuples[{Range[n], Range[m]}];
  RelationGraph[radialPredStrict[n], vertices, VertexCoordinates -> Map[radialClockCoord[n], vertices], opts]
]


(* ::Section:: *)
(*Plotting modes*)

PackageExport["PlotGraphVector"]

PlotGraphVector[graph_, vector_, opts___] := GraphPlot[graph,
  EdgeShapeFunction -> "Line", EdgeStyle -> LightGray,
  VertexShape -> MapThread[
    #1 -> ComplexDisk[#2, 20, 1]&,
    {VertexList[graph], vector}
  ],
  opts, VertexLabels -> None
];

PackageExport["PlotGraphModes"]

complexSortingValue[c_] := {-Round[Abs[c], 0.01], Mod[Arg[c+ $MachineEpsilon * I], 2Pi]}

PlotGraphModes[graph_Graph, k_:4, columns:_Integer:4, opts:OptionsPattern[Graph]] := Scope[
  vertices = VertexList[graph];
  trans = N @ ToMarkovMatrix[graph];
  {values, vectors} = Eigensystem[trans, k];
  ordering = OrderingBy[values, complexSortingValue];
  values = Part[values, ordering];
  vectors = Part[vectors, ordering];
  vectors = Map[Normalize[#, Max[Abs[#]]&]&, vectors];
  opts2 = Options[graph];
  If[OptionValue[VertexLabels] === Automatic,
    opts = DeleteCases[opts, VertexLabels -> _],
    opts2 = DeleteCases[opts2, VertexLabels -> _]
  ];
  opts2 = Sequence @@ opts2; i = 1;
  plots = MapThread[
    Column[{
      PlotGraphVector[graph, #1, opts, opts2, ImagePadding -> {{5,  5}, {10, 5}}],
      Style[TextString[NumberForm[Chop @ #2, 3]] // StringReplace[" " -> ""], If[Abs[#2] == 1.0, Bold, {}]],
      ComplexGraphicalRow[#2, 25],
      Row[{"#", i++}, BaseStyle -> Gray]
    }, Alignment->Center, Spacings -> 0]&,
    {vectors, values}
  ];
  Multicolumn[plots, columns, Spacings -> {2,0.5}, Appearance->"Horizontal", Alignment -> Center]
];


PackageExport["FastGraph3D"]

FastGraph3D[g_, opts___] := Graph3D[VertexList[g], EdgeList[g], EdgeShapeFunction -> "Point", VertexShapeFunction -> "Line"]


cyclicPairs[first_, vertices___] := Partition[{first, vertices, first}, 2, 1];

PackageExport["DirectedCycle"]
PackageExport["UndirectedCycle"]

DirectedCycle[vertices___] := Splice[DirectedEdge @@@ cyclicPairs[vertices]];
UndirectedCycle[vertices___] := Splice[UndirectedEdge @@@ cyclicPairs[vertices]];


PackageExport["DirectedPath"]
PackageExport["UndirectedPath"]

DirectedPath[vertices___] := Splice[DirectedEdge @@@ Partition[List @ vertices, 2, 1]];
UndirectedPath[vertices___] := Splice[UndirectedEdge @@@ Partition[List @ vertices, 2, 1]];


PackageExport["Clique"]

Clique[vertices___] := Splice[UndirectedEdge @@@ Subsets[{vertices}, {2}]];


PackageExport["ExploreGraph"]

Options[ExploreGraph] = Join[
  {"MaxVertices" -> Infinity, "MaxEdges" -> Infinity, "MaxDepth" -> Infinity, DirectedEdges -> True},
  DeleteCases[Options[Graph], DirectedEdges -> _]
];

stackPushList[stack_, list_] := Scan[item |-> stack["Push", item], list];

ExploreGraph[f_, initialVertices_, opts:OptionsPattern[]] := Scope[
  thisGenVertices = CreateDataStructure["Stack"];
  nextGenVertices = CreateDataStructure["Stack"];
  visitedVertices = CreateDataStructure["HashSet"];
  seenVertices = CreateDataStructure["HashSet"];
  thisGenSuccessors = <||>; allSuccessors = {};
  UnpackOptions[maxVertices, maxEdges, maxDepth, directedEdges];
  stackPushList[thisGenVertices, initialVertices];
  seenVertices["Union", initialVertices];
  edgeGenerations = {}; edgeCount = 0; generation = 0;
  While[And[visitedVertices["Length"] <= maxVertices, edgeCount <= maxEdges, generation <= maxDepth],
    If[thisGenVertices["EmptyQ"],
      generation += 1;
      AppendTo[allSuccessors, thisGenSuccessors]; thisGenSuccessors = <||>;
      If[nextGenVertices["EmptyQ"], Break[]];
      Swap[thisGenVertices, nextGenVertices]];
    vertex = thisGenVertices["Pop"];
    visited["Insert", vertex];
    successors = f[vertex];
    If[!ListQ[successors], Break[]];
    thisGenSuccessors[vertex] = successors;
    successors = Discard[successors, seenVertices["MemberQ", #]&];
    seenVertices["Union", successors];
    stackPushList[nextGenVertices, successors];
  ];
  edgeSymbol = If[directedEdges, DirectedEdge, UndirectedEdge];
  result = Flatten @ Map[succ |-> KeyValueMap[{from, to} |-> Map[edgeSymbol[from, #]&, to], succ], allSuccessors];
  If[undirectedEdges, result = DeleteDuplicatesBy[result, Sort]];
  Graph[result, GeneralUtilities`FilterOptions[opts]]
];


PackageExport["ShowLabels"]

ShowLabels[e_] := Graph[e, VertexLabels -> "Name"];


PackageExport["GraphComponentPlot"]

GraphComponentPlot[graph_] := Map[GraphPlot[#, ImageSize -> 400]&, ConnectedGraphComponents[graph]];


PackageExport["GraphComponentPlot3D"]

GraphComponentPlot3D[graph_] := Map[GraphPlot3D[#, ImageSize -> 400]&, ConnectedGraphComponents[graph]];


PackageExport["GraphEmbeddingGallery"]

$layouts = {
  "GravityEmbedding", "HighDimensionalEmbedding", "PlanarEmbedding",
  "SpectralEmbedding", "SpringElectricalEmbedding", "SpringEmbedding", "TutteEmbedding"
};

GraphEmbeddingGallery[g_Graph] := Table[Graph[g, GraphLayout -> layout, PlotLabel -> layout], {layout, $layouts}]


PackageExport["GraphVertexAssignments"]

SetUsage @ "
GraphVertexAssignments[graph$, integers$] assigns values from integers$ to the vertices of graph in all possible ways,
returning lists of assignments in the vertex order. The default value of 0 is implied.
The automorphism group of the graph will be used to return only one representative of each assignment.
"

GraphVertexAssignments[graph_, values_] := Scope[
  n = VertexCount[graph];
  values = PadRight[values, n];
  perms = Permutations[values];
  group = GraphAutomorphismGroup[graph];
  DeleteDuplicates[perms, MemberQ[Permute[#2, group], #1]&]
]