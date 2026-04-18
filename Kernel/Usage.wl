Package["WolframInstitute`PersistenceRewriting`"]

EmeLifetimes::usage =
    "EmeLifetimes[ecg] returns {eme, birth, death} triples from an EvolutionCausalGraph. \
EmeLifetimes[wmeo] extracts lifetimes from a WolframModelEvolutionObject.";

EmeLifeDurations::usage =
    "EmeLifeDurations[ecg] returns the lifetime durations of emes tracked through an EvolutionCausalGraph or WolframModelEvolutionObject.";

EmeDecayRate::usage =
    "EmeDecayRate[ecg] fits an exponential distribution to eme lifetime durations and returns the decay rate \[Lambda].";

LifeDurations::usage =
    "LifeDurations[lifetimes] returns the durations (death - birth) for a list of {eme, birth, death} triples.";

DecayRate::usage =
    "DecayRate[durations] fits an exponential distribution and returns the decay rate \[Lambda].";

FollowVertexEmes::usage =
    "FollowVertexEmes[ecg, emes] returns the forward chain of states through which the given vertex emes persist.";

BacktrackVertexEmes::usage =
    "BacktrackVertexEmes[ecg, emes] returns the backward chain of states from which the given vertex emes descend.";

FindStateEvolutionPath::usage =
    "FindStateEvolutionPath[ecg] returns a random source-to-sink chain of states (a single-way branch) through the state subgraph of an EvolutionCausalGraph.";

RestrictEvolutionCausalGraph::usage =
    "RestrictEvolutionCausalGraph[ecg, states] returns the subgraph of ecg restricted to a given list of states and the events connecting them.";

AllStateEvolutionPaths::usage =
    "AllStateEvolutionPaths[ecg] enumerates every source-to-sink state path through an EvolutionCausalGraph. AllStateEvolutionPaths[ecg, maxPaths] caps the number of paths per source-sink pair.";

SubstitutionSystemTokenTracking::usage =
    "SubstitutionSystemTokenTracking[rule, init, steps] tracks character tokens through sequential string rewriting.";

SubstitutionSystemCausalGraph::usage =
    "SubstitutionSystemCausalGraph[rule, init, steps] returns the causal graph from string substitution token tracking.";

LifeDurationsHistogram::usage =
    "LifeDurationsHistogram[durations] plots a histogram of EME lifetimes with optional exponential fit overlay.";

LifetimesBarcode::usage =
    "LifetimesBarcode[lifetimes] creates a persistence barcode visualization of {eme, birth, death} triples.";

EventInteractionDiagram::usage =
    "EventInteractionDiagram[system, event] traces backward/forward EME chains through a specific event.";

EventInteractionDiagramPlot::usage =
    "EventInteractionDiagramPlot[system, event] visualizes the event interaction diagram as a layered graph.";
