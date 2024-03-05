class A {
    foo: string = '';
}

class B extends A {
    bar: string = '';
}

type K = keyof B;
'foo' as K; // error: $Keys does not include props from super class, which is different from TS
'bar' as K; // ok

import type {K as K2} from './exported'
'foo' as K2; // error: $Keys does not include props from super class, which is different from TS
'bar' as K2; // ok
