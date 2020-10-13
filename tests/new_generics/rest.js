//@flow

function f<X: {}, Y: {}>(x: {...X}, y: {...Y}, both: {...X, ...Y}) {
  (x: $Diff<{...X, ...Y}, {...Y}>); // should be ok
  (y: $Diff<{...X, ...Y}, {...Y}>); // nope
}

function g<X: {}, Y: {x: number}>(o: {...X, ...Y}) {
  var {x, ...rest} = o;
  (rest: {...X, ...Y}); //no, because x is missing
  ({...rest, x: 42}: {...X, ...Y}); // ok
  (rest: {...X}); // no, because X's props could have been overwritten
}
