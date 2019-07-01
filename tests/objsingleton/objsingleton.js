// @flow

declare function onAction<T, Result>(
  myEnum: T,
  pattern: $ObjSingleton<T, () => Result>
): Result;

declare var action: "Action1" | "Action2" | "Action3";

// no error
const result: string = onAction(action, {
  Action1: () => "",
  Action2: () => "",
  Action3: () => ""
});

// error
const result2: number = onAction(action, {
  Action1: () => 0,
  Action2: () => 1
});

type A = "foo";
type B = "bar";

// ok
const a: $ObjSingleton<A | B, string> = { foo: "foo", bar: "bar" };
const b: {|
  foo: string,
  bar: string,
|} = a;

// ok
const c: {|foo: string, bar: string|} = { foo: "foo", bar: "bar" };
const d: $ObjSingleton<'foo' | 'bar', string> = c;

// error
const e: {foo: string, bar: string} = { foo: "foo", bar: "bar" };
const f: $ObjSingleton<'foo' | 'bar', string> = e;

// ok
const g: $ObjSingleton<'foo' | 'bar', string> = { foo: "foo", bar: "bar" };
const h: {foo: string, bar: string} = g;
