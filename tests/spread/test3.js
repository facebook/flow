var p = { y: "" };
var q = { z: "" };
var o = {
  x: 5,
  ...p,
  ...q,
};
var y: number = o.y;
var z: number = o.z;
