function remove<A, B>(a: A): [$Rest<A, {p: B}>, B] {
  const {...o, p} = a;
  return [o, p];
}

const [o, p] = remove({x: 'foo', p: 42});
(o: {|x: string|});
(p: number);
