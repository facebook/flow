function keyMirror<T: {...}>(obj: T): $KeyMirror<T> {
  const ret: $KeyMirror<T> = {};
  for (const key in obj) {
    if (!obj.hasOwnProperty(key)) {
      continue;
    }
    ret[key] = key; // error: no indexed type
  }
  return ret;
}

module.exports = keyMirror;
