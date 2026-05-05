// @flow

declare class C<T extends string> { f: T }

// $FlowExpectedError[incompatible-type]
declare var d: C<number>;

declare export var x: typeof d.f;
