import type { Op } from './defs';
import { op } from './defs';

const obj = {[op]: 'abc'};

obj as $ReadOnly<{[Op]: string}>;
obj as {[Op]: string};

obj as {[string]: string}; // error invariant position
obj as {['special']: string}; // error Op is opaque - no knowledge of specific value
