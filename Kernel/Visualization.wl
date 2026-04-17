Package["WolframInstitute`PersistenceRewriting`"]

(* ── LifeDurationsHistogram ── *)

Options[LifeDurationsHistogram] = {
  "Bins" -> Automatic,
  "ShowDistribution" -> True,
  ImageSize -> Large
};

LifeDurationsHistogram[ durations_List, opts : OptionsPattern[] ] :=
  Module[{ lambda, showDist, bins, imgSize, maxDuration, binEdges, barColor, pdfColor },
    showDist = OptionValue[ "ShowDistribution" ];
    bins = OptionValue[ "Bins" ];
    imgSize = OptionValue[ ImageSize ];

    lambda = DecayRate[ durations ];
    maxDuration = Max[ durations ];

    barColor = RGBColor[ 0.7, 0.85, 0.95 ];
    pdfColor = RGBColor[ 0.2, 0.5, 0.3 ];

    binEdges = If[ bins === Automatic,
      Range[ 0, Max[ Ceiling[ maxDuration ], 1 ] + 1, 1 ],
      bins
    ];

    Show[
      Histogram[ durations, {binEdges}, "PDF",
        ChartStyle -> barColor,
        Frame -> False,
        Axes -> False
      ],
      If[ showDist && maxDuration > 0,
        Plot[
          PDF[ ExponentialDistribution[ lambda ], x ],
          {x, 0, Max[ Ceiling[ maxDuration ], 1 ]},
          PlotStyle -> {Thick, pdfColor}
        ],
        {}
      ],
      ImageSize -> imgSize
    ]
  ];

(* ── LifetimesBarcode ── *)

Options[LifetimesBarcode] = {
  "ColorScheme" -> "Pastel",
  "BarHeight" -> 0.25,
  "BarSpacing" -> 0.05,
  "ShowLabels" -> True,
  "MinBarWidth" -> 0.2,
  ImageSize -> Large,
  AspectRatio -> Automatic
};

LifetimesBarcode[ lifetimes_List, opts : OptionsPattern[] ] :=
  Module[{ maxTime, colors, barHeight, barSpacing, showLabels, imgSize, aspectRatio,
           colorScheme, minBarWidth },
    colorScheme = OptionValue[ "ColorScheme" ];
    barHeight = OptionValue[ "BarHeight" ];
    barSpacing = OptionValue[ "BarSpacing" ];
    showLabels = OptionValue[ "ShowLabels" ];
    minBarWidth = OptionValue[ "MinBarWidth" ];
    imgSize = OptionValue[ ImageSize ];
    aspectRatio = OptionValue[ AspectRatio ];

    maxTime = Max[ lifetimes[[ All, 3 ]] ];

    colors = ColorData[ colorScheme ][ Mod[ # / Length[ lifetimes ], 1 ] ] & /@ Range[ Length[ lifetimes ] ];

    Graphics[
      MapIndexed[
        {lifetime, idx} |->
          Module[{ label, startTime, endTime, y, effectiveEndTime },
            {label, startTime, endTime} = lifetime;
            y = -(First[ idx ] - 1) * (barHeight + barSpacing);

            effectiveEndTime = If[ endTime == startTime, startTime + minBarWidth, endTime ];

            {
              colors[[ First[ idx ] ]],
              Rectangle[ {startTime, y - barHeight / 2}, {effectiveEndTime, y + barHeight / 2} ],
              If[ showLabels,
                { Black,
                  Text[
                    Style[ ToString[ label, StandardForm ], Bold ],
                    {startTime + (effectiveEndTime - startTime) / 2, y}
                  ] },
                Nothing
              ]
            }
          ],
        lifetimes
      ],
      PlotRange -> {{0, maxTime + 0.5},
                    {-(Length[ lifetimes ] - 1) * (barHeight + barSpacing) - barHeight / 2 - 0.1,
                     barHeight / 2 + 0.1}},
      AspectRatio -> If[ aspectRatio === Automatic,
        (Length[ lifetimes ] * (barHeight + barSpacing)) / (maxTime + 3),
        aspectRatio
      ],
      Frame -> {{False, False}, {True, False}},
      FrameTicks -> {{None, None}, {Range[ 0, Ceiling[ maxTime ] ], None}},
      ImageSize -> imgSize
    ]
  ];
