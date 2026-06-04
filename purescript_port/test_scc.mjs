import { init } from './decode_externs.mjs';
await init();
const Graph = await import('./output/Data.Graph/index.js');
const Tuple = await import('./output/Data.Tuple/index.js');
const Ord = await import('./output/Data.Ord/index.js');

// Test: two isolated nodes [A, M]
// Haskell should give [M, A] (reverse of input order for isolated nodes)
const verts = [
  new Tuple.Tuple(new Tuple.Tuple("A", "A"), new Tuple.Tuple("A", [])),
  new Tuple.Tuple(new Tuple.Tuple("M", "M"), new Tuple.Tuple("M", [])),
];
const result = Graph.stronglyConnCompR(Ord.ordString)(verts);
console.log("Two isolated nodes [A, M]:");
console.log("Expected: [M, A] (Haskell order)");
console.log("Got:", result.map(scc => {
  if (scc instanceof Graph.AcyclicSCC) return "AcyclicSCC(" + scc.value0.value0 + ")";
  return "CyclicSCC([" + scc.value0.map(x => x.value0) + "])";
}));
