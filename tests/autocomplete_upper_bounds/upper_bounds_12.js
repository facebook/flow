//@flow

type Id<T> = T extends unknown ? T : empty;

type Union = Id<"foo" | "bar" | "baz">

declare const f : Union => void

f(
//^
