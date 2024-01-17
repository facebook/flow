//@flow

type Id<A> = A

type Union =
| 'foo'
| 'bar'
| Id<'baz'>

declare var f : Union => void

f(
//^
