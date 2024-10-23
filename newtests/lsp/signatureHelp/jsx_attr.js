// @flow

import * as React from "react";

type PropsA = $ReadOnly<{ foo: number, bar?: string }>;
type PropsB = $ReadOnly<{ foo: boolean, baz?: string }>;
type PropsC = $ReadOnly<{ foo?: string, baz?: string }>;

type PropsPoly<P> = $ReadOnly<{ foo: P, bar?: P }>;

type PropsOmit = $ReadOnly<Omit<PropsA, 'bar'>>;
type PropsMapped = $ReadOnly<{[key in keyof PropsA]: () => PropsA[key]}>;

type PropsSpread = $ReadOnly<{...PropsA, ...PropsC}>;

function test1(Foo: React$AbstractComponent<PropsA>) {;
    return <Foo foo={/* here */} />;
}

function test2(Foo: component (foo: number, bar?: string)) {
    return <Foo foo={/* here */} />;
}

function test3(Foo: component(...props: {...PropsA})) {
    return <Foo foo={/* here */} />;
}

function test4() {
    declare class Foo extends React.Component<PropsA> {}
    return <Foo foo={/* here */} />;
}

function test5(Foo: React$AbstractComponent<PropsA>) {
    return <Foo foo={/* here */} />;
}

function test6<P>(Foo: React$AbstractComponent<PropsPoly<P>>): any {
    return <Foo foo={/* here */} />;
}

function test7<P>(Foo: React$AbstractComponent<{...PropsA, ...PropsPoly<P>}>): any {
    return <Foo foo={/* here */} />;
}

function test8<P: React$AbstractComponent<PropsA>>(Foo: P): any {
    return <Foo foo={/* here */} />;
}

function test9<P: PropsA>(Foo: React$AbstractComponent<P>): any {
    return <Foo foo={/* here */} />;
}

function testUnion1(Foo: React$AbstractComponent<PropsA | PropsA>): any {
    return <Foo foo={/* here */} />;
}

function testUnion2(Foo: React$AbstractComponent<PropsA | PropsB>): any {
    return <Foo foo={/* here */} />;
}

function testUnion3(Foo: React$AbstractComponent<PropsA | PropsC>): any {
    return <Foo foo={/* here */} />;
}

function testIntersection1(Foo: React$AbstractComponent<PropsA & PropsB>): any {
    return <Foo foo={/* here */} />;
}

function testIntersection2(Foo: React$AbstractComponent<PropsA & (PropsB & PropsC)>): any {
    return <Foo foo={/* here */} />;
}

function testIntersection3(Foo: React$AbstractComponent<PropsA | (PropsB & PropsC)>): any {
    return <Foo foo={/* here */} />;
}

function testIntersection4(Foo: React$AbstractComponent<PropsA & (PropsB | PropsC)>): any {
    return <Foo foo={/* here */} />;
}

function testOmit(Foo: React$AbstractComponent<PropsOmit>) {;
    return <Foo foo={/* here */} />;
}

function testMapped(Foo: React$AbstractComponent<PropsMapped>) {;
    return <Foo foo={/* here */} />;
}

function testSpread(Foo: React$AbstractComponent<PropsSpread>) {;
    return <Foo foo={/* here */} />;
}
