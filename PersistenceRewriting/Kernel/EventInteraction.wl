Package["WolframInstitute`PersistenceRewriting`"]

PackageScope[sanitizeVertex]
PackageScope[buildInteractionGraph]

(* ── SubstitutionSystem path ── *)

EventInteractionDiagram[ tracking_Association, event_Integer ] :=
  With[{
    lifetimes = tracking[ "Lifetimes" ],
    eventCount = tracking[ "EventCount" ]
  },
  {
    inputEmes = Select[ lifetimes, #[[ 3 ]] === event & ],
    outputEmes = Select[ lifetimes, #[[ 2 ]] === event & ]
  },
  {
    inputChains = Map[
      eme |-> { eme[[ 1 ]], traceBackwardSS[ lifetimes, eme ] },
      inputEmes ],
    outputChains = Map[
      eme |-> { eme[[ 1 ]], traceForwardSS[ lifetimes, eme ] },
      outputEmes ]
  },
    <|
      "Event" -> event,
      "InputEmes" -> inputChains,
      "OutputEmes" -> outputChains,
      "Graph" -> buildInteractionGraph[ event, inputChains, outputChains ]
    |>
  ]

traceBackwardSS[ lifetimes_, eme_ ] :=
  If[ eme[[ 2 ]] === 0, { 0 },
    With[{ parent = SelectFirst[ lifetimes, #[[ 3 ]] === eme[[ 2 ]] & ] },
      If[ MissingQ[ parent ], { eme[[ 2 ]] },
        Append[ traceBackwardSS[ lifetimes, parent ], eme[[ 2 ]] ]
      ]
    ]
  ]

traceForwardSS[ lifetimes_, eme_ ] :=
  If[ eme[[ 3 ]] === Infinity, { Infinity },
    With[{ child = SelectFirst[ lifetimes, #[[ 2 ]] === eme[[ 3 ]] & ] },
      If[ MissingQ[ child ], { eme[[ 3 ]] },
        Prepend[ traceForwardSS[ lifetimes, child ], eme[[ 3 ]] ]
      ]
    ]
  ]

(* ── WolframModel path ── *)

EventInteractionDiagram[ wmeo_, event_Integer ] /; Head[ wmeo ] =!= Association :=
  With[{
    creators = wmeo[ "EdgeCreatorEventIndices" ],
    destroyers = wmeo[ "EdgeDestroyerEventIndices" ],
    edges = wmeo[ "AllEventsEdgesList" ]
  },
  {
    inputEdgeIndices = Flatten @ Position[ destroyers, event ],
    outputEdgeIndices = Flatten @ Position[ creators, event ]
  },
  {
    inputChains = Map[
      idx |-> { edges[[ idx ]], chainBackwardWM[ creators, destroyers, idx ] },
      inputEdgeIndices ],
    outputChains = Map[
      idx |-> { edges[[ idx ]], chainForwardWM[ creators, destroyers, idx ] },
      outputEdgeIndices ]
  },
    <|
      "Event" -> event,
      "InputEmes" -> inputChains,
      "OutputEmes" -> outputChains,
      "Graph" -> buildInteractionGraph[ event, inputChains, outputChains ]
    |>
  ]

chainBackwardWM[ creators_, destroyers_, edgeIdx_ ] :=
  With[{ creatorEvent = creators[[ edgeIdx ]] },
    If[ creatorEvent === 0, { 0 },
      With[{ parentIdx = SelectFirst[ Flatten @ Position[ destroyers, creatorEvent ], True & ] },
        If[ MissingQ[ parentIdx ], { creatorEvent },
          Append[ chainBackwardWM[ creators, destroyers, parentIdx ], creatorEvent ]
        ]
      ]
    ]
  ]

chainForwardWM[ creators_, destroyers_, edgeIdx_ ] :=
  With[{ destroyerEvent = destroyers[[ edgeIdx ]] },
    If[ destroyerEvent === Infinity, { Infinity },
      With[{ childIdx = SelectFirst[ Flatten @ Position[ creators, destroyerEvent ], True & ] },
        If[ MissingQ[ childIdx ], { destroyerEvent },
          Prepend[ chainForwardWM[ creators, destroyers, childIdx ], destroyerEvent ]
        ]
      ]
    ]
  ]

(* ── EvolutionCausalGraph path ── *)

Options[EventInteractionDiagram] = { "EmeType" -> "Vertex" };

EventInteractionDiagram[ ecg_Graph, event_DirectedEdge, OptionsPattern[] ] :=
  With[{
    listEmes = If[ OptionValue[ "EmeType" ] === "Edge", ListEdgeEmes, ListVertexEmes ],
    causalGraph = ExtractCausalSubgraph[ ecg ]
  },
  {
    inputEmes = listEmes[ ExtractDestroyedSubgraph[ event ] ],
    outputEmes = listEmes[ ExtractCreatedSubgraph[ event ] ]
  },
  {
    inputChains = Map[
      eme |-> { eme, traceBackwardEcg[ causalGraph, event, eme, listEmes ] },
      inputEmes ],
    outputChains = Map[
      eme |-> { eme, traceForwardEcg[ causalGraph, event, eme, listEmes ] },
      outputEmes ]
  },
    <|
      "Event" -> event,
      "InputEmes" -> inputChains,
      "OutputEmes" -> outputChains,
      "Graph" -> buildInteractionGraph[ event, inputChains, outputChains ]
    |>
  ]

traceBackwardEcg[ causalGraph_, event_, eme_, listEmes_ ] :=
  With[{ predecessors = Select[ VertexInComponent[ causalGraph, event, {1} ],
      ev |-> MemberQ[ listEmes[ ExtractCreatedSubgraph[ ev ] ], eme ] ] },
    If[ predecessors === {}, { "Initial" },
      With[{ creator = First @ predecessors },
        Append[ traceBackwardEcg[ causalGraph, creator, eme, listEmes ], creator ]
      ]
    ]
  ]

traceForwardEcg[ causalGraph_, event_, eme_, listEmes_ ] :=
  With[{ successors = Select[ VertexOutComponent[ causalGraph, event, {1} ],
      ev |-> MemberQ[ listEmes[ ExtractDestroyedSubgraph[ ev ] ], eme ] ] },
    If[ successors === {}, { "Final" },
      With[{ destroyer = First @ successors },
        Prepend[ traceForwardEcg[ causalGraph, destroyer, eme, listEmes ], destroyer ]
      ]
    ]
  ]

(* ── Shared graph builder ── *)

sanitizeVertex[ v_ ] := v /. { Infinity -> "\[Infinity]", DirectedInfinity[ 1 ] -> "\[Infinity]" }

buildInteractionGraph[ event_, inputChains_, outputChains_ ] :=
  With[{
    inputEdges = Catenate @ Map[
      entry |-> With[{ fullChain = sanitizeVertex /@ Append[ entry[[ 2 ]], event ] },
        MapThread[ DirectedEdge, { Most @ fullChain, Rest @ fullChain } ]
      ],
      inputChains ],
    outputEdges = Catenate @ Map[
      entry |-> With[{ fullChain = sanitizeVertex /@ Prepend[ entry[[ 2 ]], event ] },
        MapThread[ DirectedEdge, { Most @ fullChain, Rest @ fullChain } ]
      ],
      outputChains ]
  },
    Graph[
      DeleteDuplicates @ Join[ inputEdges, outputEdges ],
      VertexLabels -> Automatic,
      GraphLayout -> "LayeredDigraph"
    ]
  ]

(* ── Plot wrapper ── *)

Options[EventInteractionDiagramPlot] = Options[EventInteractionDiagram];

EventInteractionDiagramPlot[ system_, event_, opts : OptionsPattern[] ] :=
  With[{ diagram = EventInteractionDiagram[ system, event, opts ] },
  {
    inputLeaves = sanitizeVertex /@ DeleteDuplicates @ Map[ First @ #[[ 2 ]] &, diagram[ "InputEmes" ] ],
    outputLeaves = sanitizeVertex /@ DeleteDuplicates @ Map[ Last @ #[[ 2 ]] &, diagram[ "OutputEmes" ] ],
    inputEmeLabels = Merge[ Map[ sanitizeVertex[ First[ #[[ 2 ]] ] ] -> #[[ 1 ]] &, diagram[ "InputEmes" ] ], Identity ],
    outputEmeLabels = Merge[ Map[ sanitizeVertex[ Last[ #[[ 2 ]] ] ] -> #[[ 1 ]] &, diagram[ "OutputEmes" ] ], Identity ]
  },
  {
    leafLabels = Join[
      KeyValueMap[ #1 -> Placed[ Style[ Row[ #2, "," ], 10 ], Before ] &, inputEmeLabels ],
      KeyValueMap[ #1 -> Placed[ Style[ Row[ #2, "," ], 10 ], After ] &, outputEmeLabels ]
    ]
  },
    Graph[
      EdgeList @ diagram[ "Graph" ],
      VertexStyle -> Join[
        { event -> RGBColor[ 0.95, 0.5, 0.5 ] },
        Map[ # -> RGBColor[ 1, 0.85, 0.6 ] &, inputLeaves ],
        Map[ # -> RGBColor[ 0.7, 0.95, 0.7 ] &, outputLeaves ]
      ],
      VertexLabels -> Join[ { event -> Style[ event, Bold, 12 ] }, leafLabels ],
      VertexSize -> Medium,
      EdgeStyle -> Directive[ Thick, GrayLevel[ 0.6 ] ],
      GraphLayout -> "LayeredDigraph",
      ImageSize -> Large
    ]
  ]
