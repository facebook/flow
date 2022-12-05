function remove<A, B>(a: A): [$Rest<A, {p: B}>, B] {
  const {p, ...o} = a;
  return [o, p];
}

const [o, p] = remove<_, number>({x: 'foo', p: 42});
(o: {|x: string|});
(p: number);
