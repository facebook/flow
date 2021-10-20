// @flow

const obj = { f: 1 };
const {...rest_obj} = obj;
export const x = rest_obj.f;
