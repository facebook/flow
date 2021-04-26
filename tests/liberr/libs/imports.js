// @flow

import type {t1} from 'm1'; // error toplevel import

declare module 'a' {
    declare export class A {
        m(): string;
    }
}

declare module 'b' {

    import type {t2} from 'missing_module'; // error missing module

    import typeof {A as a$A} from 'a';

    declare var A: a$A;

    declare export class D extends A {
        m(): number; // error number <: string
    }
}
