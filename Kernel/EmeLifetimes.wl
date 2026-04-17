Package["WolframInstitute`PersistenceRewriting`"]

(* ── MultiwaySystem path (EvolutionCausalGraph) ── *)

Options[EmeLifetimes] = {"TimeMeasure" -> "ComputationalSteps", "PropagateThroughEvents" -> False, "EmeType" -> "Vertex"};

EmeLifetimes[ ecg_Graph, OptionsPattern[] ] :=
  With[{
    causalGraph = ExtractCausalSubgraph[ ecg ],
    stateGraph = ExtractStatesSubgraph[ ecg ]
  },
  {computationalTime = UniversalTime[ stateGraph ]},
  {listEmes = If[ OptionValue[ "EmeType" ] === "Edge", ListEdgeEmes, ListVertexEmes ]},
  {emeTime = With[{ timeMeasure = OptionValue[ "TimeMeasure" ] },
    Which[
      timeMeasure === "ComputationalSteps", computationalTime,
      timeMeasure === "UniversalTime", Append[ (First @ VertexList @ stateGraph) -> 0 ] @
        KeyMap[ #[[ 2 ]] &, UniversalTime[ causalGraph ] ],
      ListQ[ timeMeasure ], Append[ (First @ VertexList @ stateGraph) -> 0 ] @
        KeyMap[ #[[ 2 ]] &, ProperTime[ causalGraph, timeMeasure ] ]
    ]
  ]},
    FoldPairList[
      {alive, event} |-> With[
        {
          ruleIn = listEmes[ ExtractDestroyedSubgraph[ event ] ],
          ruleOut = listEmes[ ExtractCreatedSubgraph[ event ] ]
        },
        {
          destroyed = If[ OptionValue[ "PropagateThroughEvents" ],
            Select[ alive[[ All, 1 ]], MemberQ[ ruleIn, # ] && !MemberQ[ ruleOut, # ] & ],
            ruleIn ],
          created = If[ OptionValue[ "PropagateThroughEvents" ],
            Select[ ruleOut, !MemberQ[ ruleIn, # ] & ],
            ruleOut ]
        },
        {
          Map[ Append[ emeTime[ event[[ 1 ]] ] ],
            Select[ alive, MemberQ[ destroyed, #[[ 1 ]] ] & ] ],
          Join[
            Select[ alive, !MemberQ[ destroyed, #[[ 1 ]] ] & ],
            Map[ {#, emeTime[ event[[ 2 ]] ]} &, created ]
          ]
        }
      ],
      Map[ {#, 0} &,
        listEmes[
          SelectFirst[ VertexList @ stateGraph, computationalTime[ # ] == 0 & ] ] ],
      SortBy[ VertexList @ causalGraph, computationalTime[ #[[ 1 ]] ] & ]
    ]
  ] // Catenate;

EmeLifeDurations[ ecg_, opts : OptionsPattern[EmeLifetimes] ] :=
  LifeDurations[ EmeLifetimes[ ecg, opts ] ];

EmeDecayRate[ ecg_, opts : OptionsPattern[EmeLifetimes] ] :=
  DecayRate[ EmeLifeDurations[ ecg, opts ] ];

(* ── WolframModel path ── *)

EmeLifetimes[ wmeo_, opts : OptionsPattern[] ] /; Head[ wmeo ] === Symbol["WolframModelEvolutionObject"] :=
  wolframModelEmeLifetimes[ wmeo, opts ];

Options[wolframModelEmeLifetimes] = Options[EmeLifetimes];

wolframModelEmeLifetimes[ wmeo_, OptionsPattern[] ] :=
  With[{
    creators = wmeo[ "EdgeCreatorEventIndices" ],
    destroyers = wmeo[ "EdgeDestroyerEventIndices" ],
    edges = wmeo[ "AllEventsEdgesList" ],
    timeMeasure = OptionValue[ "TimeMeasure" ],
    propagate = OptionValue[ "PropagateThroughEvents" ]
  },
  {eventTime = Append[ 0 -> 0 ] @ Which[
    timeMeasure === "ComputationalSteps",
      AssociationThread[ Range[ wmeo[ "EventsCount" ] ] -> Range[ wmeo[ "EventsCount" ] ] ],
    timeMeasure === "UniversalTime",
      UniversalTime[ wmeo[ "CausalGraph" ] ],
    ListQ[ timeMeasure ],
      ProperTime[ wmeo[ "CausalGraph" ], timeMeasure ]
  ]},
  {emeType = OptionValue[ "EmeType" ] /. Automatic -> "Edge"},
    Switch[ emeType,
      "Edge",
        Select[
          MapIndexed[ {First[ #2 ], eventTime[ #1[[ 1 ]] ], eventTime[ #1[[ 2 ]] ]} &,
            Thread[ {creators, destroyers} ] ],
          #[[ 3 ]] < Infinity &
        ],
      "Vertex",
        vertexEmeLifetimesWM[ edges, creators, destroyers, eventTime, wmeo[ "EventsCount" ], propagate ]
    ]
  ]

PackageScope[vertexEmeLifetimesWM]

vertexEmeLifetimesWM[ edges_, creators_, destroyers_, eventTime_, nEvents_, propagate_ ] :=
  With[{
    inputByEvent = PositionIndex[ destroyers ],
    outputByEvent = PositionIndex[ creators ]
  },
  {atomEvents = Catenate @ Table[
    With[{
      inputAtoms = If[ Lookup[ inputByEvent, event, {} ] === {}, {},
        Union @@ edges[[ Lookup[ inputByEvent, event, {} ] ]] ],
      outputAtoms = If[ Lookup[ outputByEvent, event, {} ] === {}, {},
        Union @@ edges[[ Lookup[ outputByEvent, event, {} ] ]] ]
    },
    {
      destroyed = If[ propagate, Complement[ inputAtoms, outputAtoms ], inputAtoms ],
      created = If[ propagate, Complement[ outputAtoms, inputAtoms ], outputAtoms ]
    },
      Join[ Thread[ {destroyed, event, -1} ], Thread[ {created, event, 1} ] ]
    ],
    {event, 0, nEvents}
  ]},
  {grouped = GroupBy[ atomEvents, First ]},
    Catenate @ Values @ Map[
      records |-> With[{
        sorted = SortBy[ records, {#[[ 2 ]], #[[ 3 ]]} & ]
      },
      {paired = Partition[ sorted, 2 ]},
        Cases[ paired, {{atom_, birthE_, 1}, {_, deathE_, -1}} :>
          {atom, eventTime[ birthE ], eventTime[ deathE ]} ]
      ],
      grouped
    ]
  ]
