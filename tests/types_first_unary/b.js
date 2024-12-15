import type {
  T1,
  T2,
  T3,
  T4,
} from './a';
const A = require('./a');
(A.P1: T1);
(A.P2: T2); // TODO compute boolean negation
(A.P3: T3);
(A.P4: T4); // TODO compute boolean negation
