// @flow

function keyMirror<T: {}>(obj: T): $ObjMapi<T, <K>(K) => K> {
  const ret: $ObjMapi<T, <K>(K) => K> = {};
  for (const key in obj) {
    if (!obj.hasOwnProperty(key)) {
      continue;
    }
    ret[key] = key; // error: no indexed type
  }
  return ret;
}

module.exports = keyMirror;
