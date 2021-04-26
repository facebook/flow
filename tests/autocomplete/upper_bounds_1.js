//@flow

type Union =
| 'foo'
| 'bar'
| 'baz'

declare var f : Union => void

f(
//^
