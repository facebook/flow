// @flow

declare class Super<T> {
  props: T,
  constructor(props : T) : Super<T>
}

class Bar extends Super<*> {
  props: { y : string };
}

export default Bar;
