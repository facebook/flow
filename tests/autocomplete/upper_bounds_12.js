//@flow

type Id = <A>(a: A) => A

type Union = $Call<Id, "foo" | "bar" | "baz">

declare var f : Union => void

f(
//^
