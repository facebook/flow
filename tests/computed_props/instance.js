// @flow

declare class Parent {
  top: true;
}

declare class Map extends Parent {
  [index: number]: () => string;
}

declare class Sub extends Map {
  x: number;
  meth(): number;
}

declare var map: Map;
declare var sub: Sub;
declare var str: string;
declare var num: number;

// Lookup prop in parent classes before the dict
(sub.top: true);
(sub['top']: true);

(sub['x']: number);
(sub['x']: string); // string ~> number

(sub['meth'](): number);
(sub['meth'](): string); // string ~> number

// Correct key type
(sub[num]: () => any);
(sub[123]: () => string);
(sub[num](): string);
(sub[num]: () => bool);
(sub[num](): bool); // string ~> bool

// Incorrect key type
(sub.a: () => string); // key: string ~> number
(sub['a']: () => string); // key: string ~> number
(sub[true](): () => bool); // key: bool ~> number, value: bool ~> string

// Indexer on class (not parent)
(map[num]: () => string);
(map[1](): string);
(map[num](): bool); // bool ~> string

// Write prop
map[0] = () => '';
map[0] = () => 1; // number ~> string
