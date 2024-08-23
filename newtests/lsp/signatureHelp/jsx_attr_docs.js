// @flow

import * as React from "react";

export type Props1 = $ReadOnly<{
    /**
     * This is documentation for Props1.foo property
     */
    foo: number,
    /**
     * This is documentation for Props1.bar property
     */
    bar?: string,
}>;

type Props2 = $ReadOnly<{
    /**
     * This is documentation for Props2.foo property
     */
    foo: string,
    /**
     * This is documentation for Props2.bar property
     */
    bar?: string,
}>;

function test1(Foo: React.AbstractComponent<Props1>): any {
    return <Foo foo={/* here */} />;
}

function test2(Foo: React.AbstractComponent<Props1 | Props2>): any {
    return <Foo foo={/* here */} />;
}
