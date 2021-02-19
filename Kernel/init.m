
If[$VersionNumber < 12.2,
  General::packagewlversion = "Package `` requires Wolfram Langauge version `` or later to operate.";
  Message[General::packagewlversion, "GraphWalking`", 12.2]
,
  If[Contexts["HyperArrays`"] === {}, Get["~/git/HyperArrays/Kernel/init.m"]];
  (*Get["~/git/GraphTools/Kernel/init.m"];*)
  Get @ FileNameJoin[{FileNameDrop @ $InputFileName, "Walks.m"}];
];