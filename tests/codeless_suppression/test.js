// $FlowFixMe
3 as string; // not suppressed due to missing code

// $FlowExpectedError
3 as string; // not suppressed due to missing code

// $FlowFixMe[incompatible-type]
3 as string;

// $FlowFixMe[]
3 as string;

// $FlowFixMe [asdfa
3 as string; // not suppressed due to missing code

let y : number | { x : number }  = 1;

// $FlowFixMe
y.x as string; // not suppressed due to missing code
