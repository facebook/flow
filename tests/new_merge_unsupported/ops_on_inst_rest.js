// @flow

declare class C { f: number }
declare var c: C;
const {...rest_c} = c;
export const x = rest_c.f;
