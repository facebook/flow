declare const x: number;

const e = match (x) { // OK: no refinement info on internal binding
  1 => true,
  2 as const a => true, // OK: no refinement info on internal binding
  _ => false,
};
