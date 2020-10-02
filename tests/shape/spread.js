declare var p: $Shape<{ x: number, ... }>;
const o = { y: 0, x: 0, ...p, }; // error: cannot determine type inexact
