//@flow

function f<X: {}, Y: {}>(x: {...X}, y: {...Y}, both: {...X, ...Y}) {
  x as Omit<{...X, ...Y}, keyof Y>; // nope
  y as Omit<{...X, ...Y}, keyof Y>; // nope
}

function g<X: {}, Y: {x: number}>(o: {...X, ...Y}) {
  var {x, ...rest} = o;
  rest as {...X, ...Y}; //no, because x is missing
  ({...rest, x: 42}) as {...X, ...Y}; // ok
  rest as {...X}; // no, because X's props could have been overwritten
}
