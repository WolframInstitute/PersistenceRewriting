Package["WolframInstitute`PersistenceRewriting`"]

SubstitutionSystemTokenTracking[ rule_Rule, init_String, steps_Integer ] :=
  Module[{ state, tokens, nextID, lifetimes, causalEdges, creatorOf, eventCount },
    state = init;
    tokens = MapIndexed[ {First[ #2 ], #1, 0} &, Characters[ init ] ];
    nextID = StringLength[ init ] + 1;
    lifetimes = {};
    causalEdges = {};
    creatorOf = AssociationThread[ Range[ StringLength[ init ] ] -> ConstantArray[ 0, StringLength[ init ] ] ];
    eventCount = 0;

    Do[
      With[{ matchPos = StringPosition[ state, rule[[ 1 ]], 1 ] },
        If[ matchPos === {}, Break[] ];
        With[{
          pos = First[ matchPos ],
          lhsLen = StringLength[ rule[[ 1 ]] ],
          rhsLen = StringLength[ rule[[ 2 ]] ]
        },
          eventCount++;
          With[{
            destroyedIndices = Range[ pos[[ 1 ]], pos[[ 2 ]] ],
            destroyedTokens = tokens[[ pos[[ 1 ]] ;; pos[[ 2 ]] ]]
          },
            lifetimes = Join[ lifetimes, Map[ {#[[ 1 ]], #[[ 3 ]], eventCount} &, destroyedTokens ] ];
            causalEdges = Join[ causalEdges,
              DeleteDuplicates @ Cases[ destroyedTokens,
                {id_, _, cr_} /; cr > 0 :> DirectedEdge[ cr, eventCount ] ]
            ];
            With[{
              newTokens = MapIndexed[ {nextID + First[ #2 ] - 1, #1, eventCount} &, Characters[ rule[[ 2 ]] ] ]
            },
              tokens = Join[
                tokens[[ ;; pos[[ 1 ]] - 1 ]],
                newTokens,
                tokens[[ pos[[ 2 ]] + 1 ;; ]]
              ];
              Do[ creatorOf[ nextID + i - 1 ] = eventCount, {i, rhsLen} ];
              nextID += rhsLen;
              state = StringReplacePart[ state, rule[[ 2 ]], pos ]
            ]
          ]
        ]
      ],
      {steps}
    ];

    lifetimes = Join[ lifetimes, Map[ {#[[ 1 ]], #[[ 3 ]], Infinity} &, tokens ] ];

    <|
      "Lifetimes" -> lifetimes,
      "CausalGraph" -> If[ eventCount > 0,
        Graph[ Range[ eventCount ], DeleteDuplicates[ causalEdges ] ],
        Graph[ {}, {} ]
      ],
      "EventCount" -> eventCount
    |>
  ]

SubstitutionSystemCausalGraph[ rule_Rule, init_String, steps_Integer ] :=
  SubstitutionSystemTokenTracking[ rule, init, steps ][ "CausalGraph" ]

Options[SubstitutionSystemEmeLifetimes] = {"TimeMeasure" -> "ComputationalSteps"};

PackageExport[SubstitutionSystemEmeLifetimes]

SubstitutionSystemEmeLifetimes[ rule_Rule, init_String, steps_Integer, OptionsPattern[] ] :=
  With[{ tracking = SubstitutionSystemTokenTracking[ rule, init, steps ] },
  {timeMeasure = OptionValue[ "TimeMeasure" ]},
  {eventTime = Append[ 0 -> 0 ] @ Which[
    timeMeasure === "ComputationalSteps",
      AssociationThread[ Range[ tracking[ "EventCount" ] ] -> Range[ tracking[ "EventCount" ] ] ],
    timeMeasure === "UniversalTime",
      ProperTime[ tracking[ "CausalGraph" ] ],
    ListQ[ timeMeasure ],
      ProperTime[ tracking[ "CausalGraph" ], timeMeasure ]
  ]},
    Select[
      Map[ {#[[ 1 ]], eventTime[ #[[ 2 ]] ], eventTime[ #[[ 3 ]] ]} &, tracking[ "Lifetimes" ] ],
      #[[ 3 ]] < Infinity &
    ]
  ]
