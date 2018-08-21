import * as React from "react";

class A extends React.Component<{foo: string}, {bar: string}> {
    static getDerivedStateFromProps(props, state) {
        let num: number;
        num = props.foo; // error: string ~> number
        num = state.bar; // error: string ~> number

        let str: string;
        str = props.foo; // ok
        str = state.bar; // ok

        return {bar: "baz"}; // ok
    }

    getSnapshotBeforeUpdate(prevProps, prevState) {
        let num: number;
        num = prevProps.foo; // error: string ~> number
        num = prevState.bar; // error: string ~> number

        let str: string;
        str = prevProps.foo; // ok
        str = prevState.bar; // ok

        return null;
    }
}

class B extends React.Component<void, {foo: number, bar: string}> {
    static getDerivedStateFromProps(props, state) {
        return {foo: 1}; // ok
    }

    getSnapshotBeforeUpdate(prevProps, prevState) {
        return null; // ok
    }
}

class C extends React.Component<void, {foo: number, bar: string}> {
    static getDerivedStateFromProps(props, state) {
        return {foo: "nope"}; // error: string ~> number
    }
}

class D extends React.Component<{foo: string}, {bar: string}> {
    getSnapshotBeforeUpdate(prevProps, prevState) {
        return 123; // ok
    }

    componentDidUpdate(prevProps, prevState, snapshot) {
        let x: number = snapshot; // ok
    }
}

class E extends React.Component<{foo: string}, {bar: string}> {
    getSnapshotBeforeUpdate(prevProps, prevState) {
        return undefined; // FIXME: should be an error but isn't
    }
}

class F extends React.Component<{foo: string}, {bar: string}> {
    static getDerivedStateFromProps(props, state) {
        return undefined; // error: undefined ~> (state shape | null)
    }
}
