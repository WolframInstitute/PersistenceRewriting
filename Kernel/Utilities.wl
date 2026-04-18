Package["WolframInstitute`PersistenceRewriting`"]

Needs["WolframInstitute`InfraCausality`"]

PackageScope[ListVertexEmes]
PackageScope[ListEdgeEmes]
PackageScope[ExtractStatesSubgraph]
PackageScope[ExtractCausalSubgraph]
PackageScope[ExtractDestroyedSubgraph]
PackageScope[ExtractCreatedSubgraph]
PackageScope[RandomChain]

(* ── Graph helpers ── *)

RandomChain[ g_, spec_ : Infinity ] :=
  First @ FindPath[ g, RandomChoice[ GetSources[ g ] ], RandomChoice[ GetSinks[ g ] ] ]

(* ── Statistics helpers ── *)

LifeDurations[ lifetimes_List ] :=
  Round[ #[[ 3 ]] - #[[ 2 ]] ] & /@ lifetimes;

DecayRate[ durations_List ] :=
  Module[{ dist },
    dist = FindDistributionParameters[ durations, ExponentialDistribution[ \[Lambda] ] ];
    \[Lambda] /. dist
  ];

(* ── EvolutionCausalGraph decomposition ── *)

ListVertexEmes[ state_ ] := Union @@ ListEdgeEmes[ state ];

ListEdgeEmes[ state_ ] := FromLinkedHypergraph[ state, "Hypergraph" ];

ExtractStatesSubgraph[ ecg_Graph ] :=
  With[{
    states = Cases[ VertexList[ ecg ], _List ],
    events = Cases[ VertexList[ ecg ], _DirectedEdge ]
  },
    Graph[
      states,
      Map[ event |-> DirectedEdge[ event[[ 1 ]], event[[ 2 ]], event ], events ]
    ]
  ];

ExtractCausalSubgraph[ ecg_Graph ] :=
  With[{ events = Cases[ VertexList[ ecg ], _DirectedEdge ] },
    Graph[ events, Select[ EdgeList[ ecg ],
      edge |-> Head[ edge[[ 1 ]] ] === DirectedEdge && Head[ edge[[ 2 ]] ] === DirectedEdge ] ]
  ];

ExtractDestroyedSubgraph[ event_ ] :=
  Extract[ event[[ 1 ]], event[[ 3 ]][ "Position" ] ];

ExtractCreatedSubgraph[ event_ ] :=
  Select[ event[[ 2 ]], edge |-> MemberQ[ event[[ 3 ]][ "Output" ], First @ edge ] ];

FindStateEvolutionPath[ ecg_Graph ] :=
  With[{ vertices = VertexList[ ecg ] },
    RandomChain[ Graph[ Cases[ vertices, {__List} ], Cases[ vertices, _DirectedEdge ] ] ]
  ];

RestrictEvolutionCausalGraph[ ecg_Graph, states_List ] :=
  With[{ events = Cases[ VertexList[ ecg ], _DirectedEdge ] },
    Subgraph[ ecg,
      Join[ states,
        Select[ events,
          MemberQ[ states, #[[ 1 ]] ] && MemberQ[ states, #[[ 2 ]] ] &
        ]
      ]
    ]
  ];

AllStateEvolutionPaths[ ecg_Graph, maxPaths_ : 200 ] :=
  With[{
    stateGraph = Graph[
      Cases[ VertexList[ ecg ], Except[ _DirectedEdge ] ],
      Cases[ VertexList[ ecg ], _DirectedEdge ]
    ]
  },
    DeleteDuplicates @ Catenate[
      FindPath[ stateGraph, #[[ 1 ]], #[[ 2 ]], Infinity, maxPaths ] & /@
        Tuples[ { GetSources[ stateGraph ], GetSinks[ stateGraph ] } ]
    ]
  ];

(* ── Eme tracing ── *)

FollowVertexEmes[ ecg_Graph, emes_List ] :=
  Most @ NestWhileList[
    states |-> DeleteDuplicates @@ (
      Cases[
        VertexOutComponent[ ecg, #, {1} ],
        edge_DirectedEdge /; !IntersectingQ[ ListVertexEmes[ ExtractDestroyedSubgraph[ edge ] ], emes ] :> edge[[ 2 ]]
      ] & /@ states
    ),
    Select[ GetSources[ ecg ], SubsetQ[ ListVertexEmes[ # ], emes ] & ],
    Length[ # ] > 0 &
  ];

BacktrackVertexEmes[ ecg_Graph, emes_List ] :=
  Most @ NestWhileList[
    states |-> DeleteDuplicates @@ (
      Cases[
        VertexInComponent[ ecg, #, {1} ],
        edge_DirectedEdge /; !IntersectingQ[ ListVertexEmes[ ExtractCreatedSubgraph[ edge ] ], emes ] :> edge[[ 1 ]]
      ] & /@ states
    ),
    Select[ GetSinks[ ecg ], SubsetQ[ ListVertexEmes[ # ], emes ] & ],
    Length[ # ] > 0 &
  ];
