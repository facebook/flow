declare class W {
  -[number | string]: number;
}
declare var w: W;
(w[0]: number); //ng
w[1] = 1.5;
w["a string"] = 2;
w[null] = 3; //ng

declare class R {
  +[number | string]: number;
}
declare var r: R;
(r[4]: number);
r[5] = 5.5; //ng
r["another string"] = 6; //ng
r[null] = 7; //ng

declare class RW {
  [number | string]: number
}
declare var rw: RW;
(rw[8]: number);
rw[9] = 9.5;
rw["yet another string"] = 10;
rw[null] = 11; //ng

const a: { +[number | string]: number } = {};
(a: { +[number]: number });

const b: { +[number]: number } = {};
(b: { +[number | string]: number }); //ng
