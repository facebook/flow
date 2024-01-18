/**
 * @flow
 */
// one error here, to verify lib errors sort to top.
var x: string = 0;

require('c') as empty; // error: boolean ~> empty
