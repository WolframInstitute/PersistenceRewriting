VerificationTest[
  PacletDirectoryLoad[ DirectoryName @ DirectoryName @ $TestFileName ];
  Needs[ "WolframInstitute`PersistenceRewriting`" ];
  True,
  True,
  TestID -> "Load"
]

VerificationTest[
  CanonicalFoliation[ Graph[ {1, 2, 3}, {1 -> 2, 2 -> 3} ] ],
  {{1}, {2}, {3}},
  TestID -> "CanonicalFoliation-Chain"
]

VerificationTest[
  UniversalTime[ Graph[ {1, 2, 3}, {1 -> 2, 1 -> 3} ] ],
  <|1 -> 0, 2 -> 1, 3 -> 1|>,
  TestID -> "UniversalTime-Fork"
]

VerificationTest[
  LifeDurations[ {{a, 0, 3}, {b, 1, 5}, {c, 2, 2}} ],
  {3, 4, 0},
  TestID -> "LifeDurations"
]
