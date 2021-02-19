Package["GraphWalking`"]


PackageImport["GeneralUtilities`"]
PackageImport["HyperArrays`"]


(* ::Subsection:: *)
(*RandomWalkEnsemble takes a transition matrix (or a graph) and iterates it for n steps on a particular inital state:*)

PackageExport["RandomWalkEnsemble"]

RandomWalkEnsemble[graph_Graph, init_, n_] :=
  RandomWalkEnsemble[ToMarkovMatrix[graph], init, n];

(* evolve for init for n steps *)
RandomWalkEnsemble[trans_ ? MatrixQ, init_, n_] :=
  NestList[Dot[trans, #]&, Developer`ToPackedArray @ N @ init, n];

(* use a random initial condition based on seed *)
RandomWalkEnsemble[trans_ ? MatrixQ, seed_Integer, n_] :=
  RandomWalkEnsemble[trans, BlockRandom[RandomReal[1, Length[trans]], RandomSeeding -> seed], n];







