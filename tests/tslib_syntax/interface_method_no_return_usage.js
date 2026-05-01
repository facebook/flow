import type {I1, I2, I3, I4} from './interface_method_no_return';

declare const i1: I1;
i1.m() as string; // OK — bare method returns implicit any
i1.n(1) as number; // OK

declare const i2: I2<string>;
i2.produce('x') as number; // OK — implicit any

declare const i3: I3;
if (i3.maybeRun) {
  i3.maybeRun(1) as string; // OK
}

declare const i4: I4;
i4.m() as string; // ERROR — number ~> string (explicit return still respected)
