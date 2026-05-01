import R1 from 'react';
import R1 from 'react'; // error
R1 = 42 as any; // error

import * as R2 from 'react';
import R2 from 'react'; // error
R2 = 42 as any; // error

import type {Node} from 'react';
const Node = 3; // ok: type-only import is in the type namespace, const is in the value namespace
Node = 4; // error: reassign-const
type Node = number; // error: same namespace as `import type`
