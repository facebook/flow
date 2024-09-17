//@flow

type Id<T> = T extends mixed ? T : empty;

type Union = Id<"foo" | "bar" | "baz">

declare var f : Union => void

f(
//^
