type Rec = ['val', number] | ['rec', Rec];
declare const x: Rec;

match (x) { // ERROR: missing `['rec', _]`
  ['val', _] => {},
}

match (x) { // ERROR: missing `['rec', ['rec', _]]`
  ['val', _] => {},
  ['rec', ['val', _]] => {},
}
