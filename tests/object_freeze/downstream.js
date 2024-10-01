const { inexact, NegNumber } = require('./object_freeze');

(inexact: {||}); // error: inexact -> exact

inexact.p = 0; // error: can't set prop on frozen object

NegNumber.foo as 1; // error -1 ~> 1
NegNumber.foo as -1; // okay
1 as typeof NegNumber.foo; // error 1 ~> -1
-1 as typeof NegNumber.foo; // okay
