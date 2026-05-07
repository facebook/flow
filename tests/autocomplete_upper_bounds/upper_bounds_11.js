//@flow

type Id<A> = A

type Union =
| 'foo'
| 'bar'
| Id<'baz'>

declare const f : Union => void

f(
//^
