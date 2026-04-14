import { x } from 'pkg';
x as number; // OK in state 1 (a.d.ts), ERROR in state 2 (b.d.ts)
x as string; // ERROR in state 1 (a.d.ts), OK in state 2 (b.d.ts)
