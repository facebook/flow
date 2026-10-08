declare const inexact: {x: number, ...};
declare const exact: {y: number};
declare const opt: {p?: number};
({...inexact, ...exact, ...opt}); // error: the inexact operand is not the adjacent one
({...inexact, ...opt}); // error
