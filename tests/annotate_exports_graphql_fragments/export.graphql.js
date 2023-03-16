// @flow

export type Foo = {|
    +f?: {|
        +g: {|
            +h: ?$ReadOnlyArray<{|
                +i: {||}
            |}>
        |};
    |}
|};

declare export function foo(): Foo;

export type Bar = {|
    +f: {|
        +g: {|
            +h: $ReadOnlyArray<{|
                +i: {||}
            |}>
        |};
    |}
|};

declare export function bar(): Bar;
