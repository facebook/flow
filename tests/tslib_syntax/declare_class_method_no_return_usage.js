import {M1, S1} from './declare_class_method_no_return';

declare const m: M1;
m.foo(1) as string; // OK — foo returns implicit any

declare const s: S1;
s.foo = 1; // OK — setter parameter is number
s.foo = 'hello'; // ERROR — string ~> number setter parameter
