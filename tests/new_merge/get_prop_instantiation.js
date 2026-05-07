// @flow

declare class C<T extends string> { f: T }

// $FlowExpectedError[incompatible-type]
declare const d: C<number>;

declare export const x: typeof d.f;
