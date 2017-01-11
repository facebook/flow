// @flow

export class Base<A, B, C> {
};

interface IFoo {}
interface IBar {}

export default class Child<A, B> extends Base<A, B, mixed> implements IFoo, IBar {
  p: number
}
