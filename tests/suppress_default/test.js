//$FlowFixMe[incompatible-type]
(3 : string);  // no error

//$FlowExpectedError[incompatible-type]
(3 : string);  // no error

/* $FlowFixMe[incompatible-type] */
(3 : string);  // no error

/** $FlowFixMe[incompatible-type] */
(3 : string);  // no error

// $FlowFixMe[incompatible-type]
(3 : string);  // no error

/**
 * $FlowFixMe[incompatible-type] - this is
 * a multiline comment
 */
(3 : string); // no error

// text before suppressor does not work $FlowFixMe[incompatible-type]
(3 : string);  // error
