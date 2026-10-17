import * as NS from './m';
import * as NS2 from './m2';

declare const cond: boolean;

const Same = cond ? NS : NS;
type S = Same.T;
1 as S;
true as S; // error

const Different = cond ? NS : NS2;
type D = Different.T; // error: a union of type definitions is not a type

declare const nullable: typeof NS | null;
type N = nullable.T; // error: `T` is missing in `null`, so `N` is not a type
1 as N;
true as N;

const CJS = require('./cjs');
type K = CJS.C;
new CJS.C() as K;
1 as K; // error
