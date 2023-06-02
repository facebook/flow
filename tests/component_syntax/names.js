
import * as React from 'react';

component Foo(a: number, ...props: {a: number}) {} // Error
<Foo a={false} /> // Ok (a is AnyT)

type BarT = {a: string} | {b: string}
component Bar(a: number, ...props: BarT) {}; // Error
<Bar a={false} /> // Ok

component Baz(a: number, ...props: {[string]: boolean}) {}; // Ok
<Baz a={false} />; // Error
<Baz a={42} /> // Ok

component Qux(a: number, ...props: {[string]: boolean, a: number}) {}; // Error
<Qux a={false} /> // Ok

type ROMSVType = {a: number}
component RunningOutOfMetasyntaxticVariables(a: number, ...props: {...ROMSVType}) {}; // Error
<RunningOutOfMetasyntaxticVariables a={false} /> // Ok

component NoRef(ref: string) {}; // Error
<NoRef />

component NoRefInSpread(...props: {ref: string}) {}; // Error
<NoRefInSpread />
