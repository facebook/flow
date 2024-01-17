//@flow

type Baz = 'baz'

type Union =
| 'foo'
| 'bar'
| Baz

declare var f : Union => void

f(
//^
