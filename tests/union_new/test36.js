function test1() {
  declare function foo<TArgs>(...args: TArgs): TArgs;
  declare const args: Array<string> | Array<string>;

  const x = foo(...args);
  x as Array<number>; // ERROR string ~> number (no Unsoundness.Any)
}
