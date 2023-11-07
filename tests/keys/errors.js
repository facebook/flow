/**
 * @format
 * @flow
 */

const O = {a: 1, b: 2};
type K = $Keys<typeof O>;
declare var k: K;
k as 'c';
'c' as K;
