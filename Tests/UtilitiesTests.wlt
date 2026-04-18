VerificationTest[
  PacletDirectoryLoad[ DirectoryName @ DirectoryName @ $TestFileName ];
  Needs[ "WolframInstitute`InfraCausality`" ];
  Needs[ "WolframInstitute`PersistenceRewriting`" ];
  True,
  True,
  TestID -> "Load"
]

VerificationTest[
  LongestPathFoliation[ Graph[ {1, 2, 3}, {1 -> 2, 2 -> 3} ] ],
  {{1}, {2}, {3}},
  TestID -> "LongestPathFoliation-Chain"
]

VerificationTest[
  ProperTime[ Graph[ {1, 2, 3}, {1 -> 2, 1 -> 3} ] ],
  <|1 -> 0, 2 -> 1, 3 -> 1|>,
  TestID -> "ProperTime-Fork"
]

VerificationTest[
  LifeDurations[ {{a, 0, 3}, {b, 1, 5}, {c, 2, 2}} ],
  {3, 4, 0},
  TestID -> "LifeDurations"
]
