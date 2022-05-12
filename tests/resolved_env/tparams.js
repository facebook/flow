//@flow

function remove<A>(a: A, x): A {
  (a: empty);
  (42: A);
  return a
}

const o: empty = remove<number>(1, 42);
const p: empty = remove(1, 42);

function remove_anno<A>(a: A): A {
  (a: empty);
  (42: A);
  return a
}

const q: empty = remove_anno<number>(1);
const r: empty = remove_anno(1);

function removex<A, B>(a: A): [$Rest<A, {p: B}>, B] {
  const {p, ...o} = a;
  return [o, p];
}

const [ox, px] = removex({x: 'foo', p: 42});
(ox: {|x: string|});
(px: number);
