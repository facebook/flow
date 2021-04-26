//@flow

type Union =
| 'foo'
| 'bar'
| 'baz'

declare var f : Union => void

var x;

x =  ;
//  ^

f(x);
