// @flow

/**
 * foo
 * @param bar - the first summand
 * @param baz - the second and third summands
 * @param [baz.x] - the second summand
 * @param [baz.y=0] - the third summand
 * @unrecognized this tag is unrecognized
 */
function foo(bar, baz) {}

foo(
