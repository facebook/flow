{
  declare var empty: {||};
  const {...others} = empty;
  others.foo = 1; // ERROR
}
{
  const {...others} = null;
  others.foo = 1; // ERROR
}
{
  class C {}
  const {...others} = new C();
  others.foo = 1; // ERROR
}

{
  class C {
    a: number;
  }

  {
    const {a, ...others} = new C();
    (others: {||}); // OK
  }
  {
    const {...others} = new C();
    (others: {|a: number|}); // OK
  }
}
{
  class C {
    a: number;
  }
  class D extends C {
    b: string;
  }

  {
    const {a, b, ...others} = new D();
    (others: {||}); // OK
  }
  {
    const {a, ...others} = new D();
    (others: {|b: string|}); // OK
  }
  {
    const {...others} = new D();
    (others: {|a: number, b: string|}); // OK
  }
}
