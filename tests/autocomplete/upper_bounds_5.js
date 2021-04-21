//@flow

type Union =
| 'foo'
| 'bar'
| 'baz'

declare var f : Union => void
declare var g : Union => void

var x;

x =  ;
//  ^

f(x);
g(x);
