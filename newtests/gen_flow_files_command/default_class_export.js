// @flow

export class Base<A, B, C> {
};

export default class Child<A, B> extends Base<A, B, mixed> {
  p: number
}
