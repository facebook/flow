
import * as React from 'react';

component Foo(a: number, ...props: {a: number}) { return <div /> } // Error
<Foo a={false} /> // Ok (a is AnyT)

type BarT = {a: string} | {b: string}
component Bar(a: number, ...props: BarT) { return <div /> }; // Error
<Bar a={false} /> // Ok

component Baz(a: number, ...props: {[string]: boolean}) { return <div /> }; // Ok
<Baz a={false} />; // Error
<Baz a={42} /> // Ok

component Qux(a: number, ...props: {[string]: boolean, a: number}) { return <div /> }; // Error
<Qux a={false} /> // Ok

type ROMSVType = {a: number}
component RunningOutOfMetasyntaxticVariables(a: number, ...props: {...ROMSVType}) { return <div /> }; // Error
<RunningOutOfMetasyntaxticVariables a={false} /> // Ok

component NoRef(ref: string) { return <div /> }; // Error
<NoRef />

component NoRefInSpread(...props: {ref: string}) { return <div /> }; // Error
<NoRefInSpread />
