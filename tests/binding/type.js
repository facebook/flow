// @flow

{
  type A = number;
  (1: A);

  {
    type A = boolean;
    (true: A);
  }

  {
    type A = string;
    ('': A);
  }

  (0: A);
}

{
  let a = "str";
  {
    type a = number;
    (1: a); // ?
  }
  ;(a: number);
}
