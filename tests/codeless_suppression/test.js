// @flow

// $FlowIssue
(3: string);

// $FlowFixMe
(3: string);

// $FlowExpectedError
(3: string);

// $FlowFixMe[incompatible-cast]
(3: string);

// $FlowFixMe[]
(3: string);

// $FlowFixMe [asdfa
(3: string);

let y : number | { x : number }  = 1;

// $FlowFixMe
(y.x : string); // no errors
