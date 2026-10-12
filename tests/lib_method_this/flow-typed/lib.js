interface Accessor<This, Value> {
  get(this: This): Value;
}

declare class Box {
  get(this: number): string;
  static make(this: number): Box;
}
