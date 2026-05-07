//@flow

type Union =
| 'foo'
| 'bar'
| 'baz'

declare const f : Union => void

f('
// ^
