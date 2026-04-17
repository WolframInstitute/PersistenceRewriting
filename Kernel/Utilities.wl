Package["WolframInstitute`PersistenceRewriting`"]

PackageScope[GetSources]
PackageScope[GetSinks]
PackageScope[ListVertexEmes]
PackageScope[ListEdgeEmes]
PackageScope[ExtractStatesSubgraph]
PackageScope[ExtractCausalSubgraph]
PackageScope[ExtractDestroyedSubgraph]
PackageScope[ExtractCreatedSubgraph]
PackageScope[RandomChain]

(* ── Graph helpers ── *)

GetSources[ g_ ] := Select[ VertexList @ g, VertexInDegree[ g, # ] == 0 & ];

GetSinks[ g_ ] := Select[ VertexList @ g, VertexOutDegree[ g, # ] == 0 & ];

RandomChain[ g_, spec_ : Infinity ] :=
  First @ FindPath[ g, RandomChoice[ GetSources[ g ] ], RandomChoice[ GetSinks[ g ] ] ]

(* ── Time measures ── *)

CanonicalFoliation[ g_Graph ] :=
  With[{
    sources = Select[ VertexList[ g ], VertexInDegree[ g, # ] == 0 & ],
    d = -GraphDistanceMatrix[ Graph[ g, EdgeWeight -> ConstantArray[ -1, EdgeCount[ g ] ] ] ]
  },
  {layerOf = AssociationThread[
    VertexList[ g ] ->
      Round[ Min[ Select[ #, dist |-> dist > -Infinity ] ] & /@
        MapThread[ List, d[[ VertexIndex[ g, sources ], All ]] ] ]
  ]},
    Values @ KeySort @ GroupBy[ Normal[ layerOf ], Last -> First ]
  ]

ProperTime[ g_Graph, foliation_List ] :=
  AssociationThread[
    Catenate[ foliation ] ->
    Catenate @ MapIndexed[ ConstantArray[ First[ #2 ] - 1, Length[ #1 ] ] &, foliation ]
  ]

UniversalTime[ g_Graph ] :=
  ProperTime[ g, CanonicalFoliation[ g ] ]

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
