/**
 * @format
 * @flow
 */

const O = {a: 1, b: 2};
type K = keyof typeof O;
declare const k: K;
k as 'c';
'c' as K;
