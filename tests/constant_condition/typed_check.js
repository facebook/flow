{
  let x1 = 'avuyv' as const
  if (x1) {} // TODO ERROR

  let x2 = '' as const
  if (x2) {} // ERROR, falls to SingletonStrT

  let x3: string = 'avuyv'
  if (x3) {} // can't error on this because we are just a type system
  if (x3 === '') {
    if (x3) {} // ERROR, refined, falsy, falls to SingletonStrT
  }

  if (x3) {
    if (x3) {} // ERROR we don't know what this is
              // but we know it's truthy because it's in the `then` statement.
  }

  if(x3==='fdsafdas'){
    if(x3) {} // TODO ERROR refined. truthy. StrGeneralT -> truthy
  }

  let x4: string = ''
  if (x4) {} // we can't error on this. just generic string

  function f() { return 'aa' as const}
  if(f()) {} // TODO ERROR
  var aaa;
  if(aaa = f()) {} // TODO ERROR

  function ff(): string { return 'aa'}
  if(ff()) {} // we can't error on this bc it's a string and we're just a type system

  type StringUnion = 'aa' | 'bb' | 'cc';
  let uu : StringUnion = 'aa'
  if(uu) {} // TODO ERROR, it's always truthy

  if(uu as string) {} // TODO ERROR because we recurs to the arg of `AsExpression`

  let ss: string = uu
  if(ss) {} // OK because `ss` is a general string

  type FunnyUnion1 = 'aa' | 'bb' | 'cc' | '';
  type FunnyUnion2 = 'aa' | 'bb' | 'cc' | 0;
  type FunnyUnion3 = 'aa' | 'bb' | 'cc' | 1;
  type FunnyUnion4 = 'aa' | 'bb' | 'cc' | 2;
  type FunnyUnion5 = 'aa' | 'bb' | 'cc' | -1;
  type FunnyUnion6 = 'aa' | 'bb' | 'cc' | null;
  type FunnyUnion7 = 'aa' | 'bb' | 'cc' | {a: number};

  let test1: FunnyUnion1 = 'aa';
  if (test1) {} // OK because `''` is falsy so FunnyUnion1 can be used in conditions

  let test2: FunnyUnion2 = 'aa';
  if (test2) {} // OK because `0` is falsy so FunnyUnion2 can be used in conditions

  let test3: FunnyUnion3 = 'aa';
  if (test3) {} // TODO ERROR because FunnyUnion3 is always truthy

  let test4: FunnyUnion4 = 'aa';
  if (test4) {} // TODO ERROR because FunnyUnion4 is always truthy

  let test5: FunnyUnion5 = 'aa';
  if (test5) {} // TODO ERROR because FunnyUnion5 is always truthy

  let test6: FunnyUnion6 = 'aa';
  if (test6) {} // OK because null is falsy so FunnyUnion6 can be used in conditions

  let test7: FunnyUnion7 = {a: 42};
  if (test7) {} // TODO ERROR because FunnyUnion7 is always truthy
}

{
  class C {
    foo: C;
    bar(): C {
      return this.foo;
    }
  }

  // Indexed access
  const a: Array<C> = [];
  if(a[0]) {} // ok because it could be `undefined`

  const b: {[string]: C} = {};
  if(b['foo']) {} // ok because it coudl be `undefined`

  // Class members
  const c: C = new C();
  if(c.foo) {} // ok because it could be `undefined`. `Member` expression is allowlisted.
  if(c.bar()) {}; // the inferred type of `InstanceT`. `InstanceT` is not turned on currently

  let d: C;
  function f(): C {
    return d; // Is undefined
  }
  if(d) {} // Currently doesn't error. turn on `NullT` & `VoidT` to give error.
  if(f()) {} // OK because f() could return undefined
}

{
  const a: Array<'fdasfads'> = [];
  let ele = a[0]
  if(ele) {} // OK because `ele` is assigned from an array access

  let n1: 3 | 4;
  function f1() { return n1; }
  let who1 = f1()
  if(who1) {} // OK because `who1` is assigned from a function call

  let n2: 0 | 1;
  function f2() { return n2; }
  let who2 = f2()
  if(who2) {} // OK because `0` is falsy and `1` is truthy.
}

{
  let x1: number = 6;
  if (x1) {
    if(x1) {} // error, truthy
  }

  if (!x1) {
    if (x1) {} // error, falsy
  }

  let x2: bigint = 6n;
  if (x2) {
    if(x2) {} // TODO error, truthy. bigint is not fully turned on for flow
  }

  if (!x2){
    if (x2) {} // TODO error, falsy. bigint is not fully turned on for flow
  }
}

{
  let x1: number = 6;
  if (x1) { if (x1++) {} } // ok, we don't check increment/decrement
  if (x1) { if (++x1) {} } // ok, we don't check increment/decrement
  if (x1) { if (x1--) {} } // ok, we don't check increment/decrement
  if (x1) { if (--x1) {} } // ok, we don't check increment/decrement
  if (x1) { if (x1+=5) {} } // ok, we don't check arithmetic assignment
  if (x1) { if (x1-=5) {} } // ok, we don't check arithmetic assignment
  if (x1) { if (x1*=5) {} } // ok, we don't check arithmetic assignment
  if (x1) { if (x1/=5) {} } // ok, we don't check arithmetic assignment
  if (x1) { if (x1%=5) {} } // ok, we don't check arithmetic assignment
  if (x1) { if (x1**=5) {} } // ok, we don't check arithmetic assignment
  if (x1) { if (x1&=5) {} } // ok, we don't check arithmetic assignment
  if (x1) { if (x1|=5) {} } // ok, we don't check arithmetic assignment
  if (x1) { if (x1^=5) {} } // ok, we don't check arithmetic assignment
  if (x1) { if (x1<<=5) {} } // ok, we don't check arithmetic assignment
  if (x1) { if (x1>>=5) {} } // ok, we don't check arithmetic assignment
  if (x1) { if (x1>>>=5) {} } // ok, we don't check arithmetic assignment
  if (x1) { if (x1??=5) {} } // ok, we don't check arithmetic assignment

}

{
  type T = 'foo' | 'bar';
  declare const x: ?T;
  x === 'foox'; // ERROR from existing check. Will be replaced by the new subtyping check

  type idk =
  | {
      ha?: string,
      jo?: string,
      ...
    }
  | boolean;
  declare const y: boolean | idk;
  y.jo == null  // no existing error
  && y.ha === 'fdas'; // existing double errors

  enum exampleEnum of string {
    A = 'a',
    B = 'b',
  };

  declare const pp: ?exampleEnum;
  switch (true) {
    case pp === exampleEnum.A: break; // should not error
    default: break;
  }
}
