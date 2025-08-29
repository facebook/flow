let y : number | { x : number }  = 1;

//$FlowFixMe[incompatible-type]
//$FlowFixMe[prop-missing]
//$FlowFixMe[arithmetic] unused
(y.x : string);

//$FlowFixMe[arithmetic] unused
//$FlowFixMe[incompatible-type]
//$FlowFixMe[prop-missing]
(y.x : string);

//$FlowFixMe[arithmetic] unused
//$FlowFixMe[incompatible-type]
//$FlowFixMe[incompatible-type] unused
//$FlowFixMe[prop-missing]
(y.x : string);

//$FlowFixMe[arithmetic] unused
//$FlowFixMe[incompatible-type] unused
//$FlowFixMe[incompatible-type] unused
//$FlowFixMe[prop-missing] unused
(y.x : string); // error not suppressed, missing code


//$FlowFixMe unused
//$FlowFixMe[arithmetic] unused
//$FlowFixMe[incompatible-type]
//$FlowFixMe[incompatible-type] unused
//$FlowFixMe[prop-missing]
(y.x : string);

//$FlowFixMe[arithmetic] unused
//$FlowFixMe[incompatible-type] unused
//$FlowFixMe unused
//$FlowFixMe[incompatible-type] unused
//$FlowFixMe[prop-missing]
(y.x : string); // error w/incompatible cast
