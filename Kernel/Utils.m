Package["GraphWalking`"]


PackageImport["GeneralUtilities`"]
PackageImport["HyperArrays`"]


(*Add the ability to BlockMap to apply to a single level only.*)

BlockMap; Unprotect[BlockMap];
(* basically calls BlockMap at a given level only *)
BlockMap[f_, array_, level_ -> n_] := Scope[
  depth = ArrayDepth[array];
  array = Transpose[array, level <-> depth];
  array = Map[f /@ Partition[#, n]&, array, {depth - 1}];
  Transpose[array, depth <-> level]
];

DownValues[BlockMap] = SortBy[DownValues[BlockMap], FreeQ[$Failed] /* Not];


(* ::Subsection:: *)
(*ArrayDecimate will take every n elements from an array. Generalizes to multiple levels.*)
(*It can also summarize 'blocks' of n elements using a given function.*)
(*Basically a general-purpose course-graining tool.*)


PackageExport["BlockMapped"]

BlockMapped::usage = "BlockMapped[f, n] summarizes blocks of n elements with the function f.";

decimate[array_, level_, n_Integer] := ArrayPart[array, Append[ConstantArray[All, level - 1], Every[n]]];
decimate[array_, level_, Into[n_]] := decimate[array, level, Ceiling[ArrayDimension[array, level] / n]];
decimate[array_, level_, 1|None] := array;
decimate[array_, level_, BlockMapped[f_, n_Integer]] := BlockMap[f, array, level -> n];

PackageExport["ArrayDecimate"]

(* single spec*)
ArrayDecimate[array_List, spec_] := decimate[array, 1, spec];

(* list of specs {spec_1, spec_2, ...} *)
ArrayDecimate[array_List, spec_List] := Scope[
  Do[array = decimate[array, i, spec[[i]]], {i, Length[spec]}];
  array
];

(* association of level \[Rule] spec *)
ArrayDecimate[array_List, spec_Association] := Scope[
  KeyValueScan[Function[array = decimate[array, #1, #2]], spec];
  array
];

(* operator form *)
ArrayDecimate[spec_][array_List] := ArrayDecimate[array, spec];



(* ::Subsection:: *)
(*The function ToMarkovMatrix will construct a Markov matrix from the the graph. *)
(*This lets us turn a space graph into the transition matrix of the random walk along that graph.*)


PackageExport["ToProbabilityVector"]

(* helper functions *)
ToProbabilityVector[list_] := Developer`ToPackedArray @ Normalize[list, Total];


PackageExport["ToMarkovMatrix"]

(* creates a markov matrix from a transition graph *)
ToMarkovMatrix[graph_Graph] :=
  Transpose @ Map[ToProbabilityVector] @ Transpose @ Normal[AdjacencyMatrix[graph]];
