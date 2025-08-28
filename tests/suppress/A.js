// $FlowFixMe[incompatible-type]
var test1: string = 123; // This error should be suppressed

function getNum() {
  return 123;
}

// $FlowFixMe[incompatible-type] This was the second loc in the error
var test2: string = getNum(); // This error should be suppressed

// $FlowFixMe[incompatible-type] Error unused suppression

var test3: string = 123; // This error is NOT suppressed

                         // $FlowFixMe[incompatible-type] Indentation shouldn't matter
var test4: string = 123; // This error should be suppressed

/*
 * $FlowFixMe[incompatible-type]
 */
var test5: string = 123;
