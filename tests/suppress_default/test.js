// @flow

//$FlowFixMe
(3 : string);  // no error

//$FlowExpectedError
(3 : string);  // no error

//$FlowIssue
(3 : string);  // no error

/* $FlowFixMe */
(3 : string);  // no error

/* $FlowIgnore */
(3 : string);  // no error

/** $FlowFixMe */
(3 : string);  // no error

// $FlowFixMe
(3 : string);  // no error

/**
 * $FlowFixMe - this is
 * a multiline comment
 */
(3 : string); // no error

// text before suppressor does not work $FlowFixMe
(3 : string);  // error
