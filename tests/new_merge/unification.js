// @flow

export class A {}
export class B extends A {}

declare export var x: Array<B/* tvar 1 */>;
