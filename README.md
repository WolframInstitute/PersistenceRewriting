# PersistenceRewriting

Track elementary multiway entities (EMEs) through rewriting system evolutions, computing lifetimes and decay statistics.

## Installation

```wolfram
PacletInstall["WolframInstitute/PersistenceRewriting"]
```

## Usage

```wolfram
Needs["WolframInstitute`PersistenceRewriting`"]

(* String substitution system *)
lifetimes = SubstitutionSystemEmeLifetimes["AB" -> "BA", "AABB", 20]
LifetimesBarcode[lifetimes]
LifeDurationsHistogram[LifeDurations[lifetimes]]

(* WolframModel *)
wmeo = ResourceFunction["WolframModel"][{{1, 2}, {2, 3}} -> {{1, 3}, {3, 4}, {4, 2}}, {{0, 0}, {0, 0}}, 10];
lifetimes = EmeLifetimes[wmeo]
```

## Testing

```bash
wolframscript -f run_tests.wls
```
