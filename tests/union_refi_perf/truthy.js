// @flow

type U = 0 | 1 | 2 | 3 | 4 | 5;

declare var x : ?U;
declare var y : U;
if (x && x !== y) { 
    (x : 1 | 2 | 3 | 4 | 5);
} 
