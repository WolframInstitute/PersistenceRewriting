VerificationTest[
  PacletDirectoryLoad[ DirectoryName @ DirectoryName @ $TestFileName ];
  Needs[ "WolframInstitute`PersistenceRewriting`" ];
  True,
  True,
  TestID -> "Load"
]

VerificationTest[
  tracking = SubstitutionSystemTokenTracking[ "AB" -> "BA", "ABA", 3 ];
  Keys[ tracking ],
  {"Lifetimes", "CausalGraph", "EventCount"},
  TestID -> "SubstitutionSystemTokenTracking-Keys"
]

VerificationTest[
  tracking = SubstitutionSystemTokenTracking[ "AB" -> "BA", "ABA", 3 ];
  tracking[ "EventCount" ] > 0,
  True,
  TestID -> "SubstitutionSystemTokenTracking-HasEvents"
]

VerificationTest[
  GraphQ @ SubstitutionSystemCausalGraph[ "AB" -> "BA", "ABA", 3 ],
  True,
  TestID -> "SubstitutionSystemCausalGraph-IsGraph"
]

VerificationTest[
  lifetimes = SubstitutionSystemEmeLifetimes[ "AB" -> "BA", "ABA", 3 ];
  MatchQ[ lifetimes, {{_, _Integer, _Integer} ...} ],
  True,
  TestID -> "SubstitutionSystemEmeLifetimes-Format"
]
