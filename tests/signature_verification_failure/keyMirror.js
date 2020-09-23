//@flow
declare function keyMirror<T: {}>(obj: T): $ObjMapi<T, <K>(K) => K>;

export const a = keyMirror({['a' + 'b']: null, b: null});

const ba = {a: 42};

export const b = keyMirror({...ba});
