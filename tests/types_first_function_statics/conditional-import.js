import {f, g} from './conditional';

f.legal as number; // OK
f.a as number; // ERROR
f.b as number; // ERROR

g.legal as number; // OK
g.a as number; // ERROR
