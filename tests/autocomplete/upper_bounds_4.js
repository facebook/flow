//@flow

type Union =
| 'foo'
| 'bar'
| 'baz'

declare var f : Union => void

var x = ('foo' : 'foo');

declare var cond : boolean;

if (cond) {
  x =  ;
//    ^
}

f(x);
