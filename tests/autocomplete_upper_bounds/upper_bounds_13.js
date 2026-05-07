//@flow

type Baz = 'baz'

type Union =
| 'foo'
| 'bar'
| Baz

declare const f : Union => void

f(
//^
