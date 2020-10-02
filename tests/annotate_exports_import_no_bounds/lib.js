// @flow

declare function keyMirror<T: {}>(obj: T): $ObjMapi<T, <K>(K) => K>;

module.exports = keyMirror;
