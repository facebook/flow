const simple = match (value) {
  1 => 'one',
  NaN => 'nan',
  _ => 'other',
};
const bound = match (getValue()) {
  [const head, ...const tail] => head,
  _ => null,
};
const empty = match (getValue()) {};
match (value) {
  {const a, b: 0} => {},
  _ => {},
}
