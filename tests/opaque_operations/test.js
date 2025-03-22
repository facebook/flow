{
  declare opaque type T1: {foo: string};
  declare const o: T1;
  ({...o}) as {foo: string}; // ok
  ({...o}) as empty; // error
}

{
  declare opaque type T2: component(foo: string);
  declare const foo: React.ElementConfig<T2>;
  foo as {+foo: string}; // ok
  foo as empty; // error
}

{
  declare opaque type T3: 'bar';
  declare const foo: {bar: string};
  declare const key: T3;
  foo[key] as string; // ok
  foo[key] as empty; // error
}
