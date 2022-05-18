//@flow

declare var x: { y?: {z: number } };

if (x.y) {
    (x.y.z: empty); //err num
}

class C { }

declare var y: mixed;
var B = null;

function set_b() {
  B = { C };
  if (y instanceof B.C) {
      (y: empty); // err C
  }
}

set_b();


if (y instanceof B?.C) { // err invalid
    (y: empty); // err C
}
