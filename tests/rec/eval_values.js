type CyclicWith$Values = {
  comment: string,
  [key: string]: $Values<CyclicWith$Values>,
};

declare const val: $Values<CyclicWith$Values>;
declare const s: string;
({ [s]: val }) as CyclicWith$Values; // error, but should not stackoverflow

import {options, type Result as BadCyclicResult} from './exported_eval_values';
options.foo as empty; // error
declare const r: BadCyclicResult;
r == 1; // no recursion limit exceeded
