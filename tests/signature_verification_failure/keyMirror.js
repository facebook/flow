//@flow
declare function keyMirror<T: {...}>(obj: T): $KeyMirror<T>;

export const a = keyMirror({['a' + 'b']: null, b: null});

const ba = {a: 42};

export const b = keyMirror({...ba});
