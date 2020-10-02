// $FlowFixMe[incompatible-type]
var test1: string = 123; // This error should be suppressed

// $FlowIssue[incompatible-type]
var test2: string = 123; // This error should be suppressed

function getNum() {
  return 123;
}

// $FlowFixMe[incompatible-type] This was the second loc in the error
var test3: string = getNum(); // This error should be suppressed

// $FlowFixMe Error unused suppression

var test4: string = 123; // This error is NOT suppressed

                         // $FlowFixMe[incompatible-type] Indentation shouldn't matter
var test5: string = 123; // This error should be suppressed

/*
 * $FlowFixMe[incompatible-type]
 */
var test6: string = 123;
