import { C } from './exported';

declare const c: C;

c._prop as number; // OK: computed key is never munged
c._meth() as number; // OK: computed method key is never munged
c._ref as number; // OK: value-reference computed key is never munged
c.kept as number; // OK: non-underscore computed key is exported

c._ident; // ERROR: non-computed identifier member is munged
