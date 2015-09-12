declare class D {
  constructor(): { x: number }; // OK
  y: any;
}

var d = new D();
d.x = ""; // error, string ~/~ number (but property x is found)

(new D: D); // error, new D doesn't have property y

module.exports = D;
