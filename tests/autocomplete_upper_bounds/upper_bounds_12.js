//@flow

type Id<T> = T extends unknown ? T : empty;

type Union = Id<"foo" | "bar" | "baz">

declare var f : Union => void

f(
//^
