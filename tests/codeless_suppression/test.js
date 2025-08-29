// $FlowFixMe
(3: string); // not suppressed due to missing code

// $FlowExpectedError
(3: string); // not suppressed due to missing code

// $FlowFixMe[incompatible-type]
(3: string);

// $FlowFixMe[]
(3: string);

// $FlowFixMe [asdfa
(3: string); // not suppressed due to missing code

let y : number | { x : number }  = 1;

// $FlowFixMe
(y.x : string); // not suppressed due to missing code
