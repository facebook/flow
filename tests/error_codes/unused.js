// @flow

let y : number | { x : number }  = 1;

//$FlowFixMe[incompatible-cast]
//$FlowFixMe[prop-missing]
//$FlowFixMe[arithmetic] unused
(y.x : string);

//$FlowFixMe[arithmetic] unused
//$FlowFixMe[incompatible-cast]
//$FlowFixMe[prop-missing]
(y.x : string);

//$FlowFixMe[arithmetic] unused
//$FlowFixMe[incompatible-cast]
//$FlowFixMe[incompatible-type] unused
//$FlowFixMe[prop-missing]
(y.x : string);

//$FlowFixMe[arithmetic] unused
//$FlowFixMe[incompatible-cast] unused
//$FlowFixMe[incompatible-type] unused
//$FlowFixMe[prop-missing] unused
// $FlowExpectedError
(y.x : string);


//$FlowFixMe unused
//$FlowFixMe[arithmetic] unused
//$FlowFixMe[incompatible-cast]
//$FlowFixMe[incompatible-type] unused
//$FlowFixMe[prop-missing]
(y.x : string);

//$FlowFixMe[arithmetic] unused
//$FlowFixMe[incompatible-cast] unused
//$FlowFixMe unused
//$FlowFixMe[incompatible-type] unused
//$FlowFixMe[prop-missing]
(y.x : string); // error w/incompatible cast
