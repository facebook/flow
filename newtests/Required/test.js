/*
 * @flow
 * @lint-ignore-every LINEWRAP1
 */

import { suite, test } from "flow-dev-tools/src/test/Tester";

export default suite(({ addFile, addFiles, addCode }) => [
  test("$Required<T>", [
    addCode(`
    type Foo = {
      field?: number
    };
    
    var x: { field: number } = {};
    var y: $Required<Foo> = x;
    (y.field: number);
    
    function f() {}
    
    var a: $Required<Foo> = f(); // error
    var b: $Required<Foo> = null; // error
    
    function baz(n: null, m: void) {}
    baz(y, y); // error
    
    declare class PointB {
      x?: number;
      y?: number;
    }
    
    declare class PointC {
      x?: number;
      y?: number;
    }
    
    type IPoint = {
      x?: number,
      y?: number
    };
    
    declare var pc: PointC;
    declare var pi: IPoint;
    declare var ps: $Required<IPoint>;
    
    (pc: IPoint);
    (pc: $Required<IPoint>); // error
    (pc: $Required<PointB>); // error
    (pc: { x?: number });
    (pc: $Required<{ x?: number }>); // error
    
    (pi: PointC); // error
    (pi: $Required<PointC>); // error
    (pi: { x?: number });
    (pi: $Required<{ x?: number }>); // error
    
    (ps: PointC); // error
    (ps: $Required<IPoint>);
    (ps: { x: number });
    (ps: $Required<{ x?: number }>); // error
    `).newErrors(
        `
          test.js:14
           14:     var a: $Required<Foo> = f(); // error
                                           ^^^ Cannot assign \`f()\` to \`a\` because undefined [1] is incompatible with \`$Required\` [2].
            References:
             12:     function f() {}
                                 ^ [1]
             14:     var a: $Required<Foo> = f(); // error
                            ^^^^^^^^^^^^^^ [2]

          test.js:15
           15:     var b: $Required<Foo> = null; // error
                                           ^^^^ Cannot assign \`null\` to \`b\` because null [1] is incompatible with \`$Required\` [2].
            References:
             15:     var b: $Required<Foo> = null; // error
                                             ^^^^ [1]
             15:     var b: $Required<Foo> = null; // error
                            ^^^^^^^^^^^^^^ [2]

          test.js:18
           18:     baz(y, y); // error
                       ^ Cannot call \`baz\` with \`y\` bound to \`n\` because \`$Required\` [1] is incompatible with null [2].
            References:
              9:     var y: $Required<Foo> = x;
                            ^^^^^^^^^^^^^^ [1]
             17:     function baz(n: null, m: void) {}
                                     ^^^^ [2]

          test.js:18
           18:     baz(y, y); // error
                          ^ Cannot call \`baz\` with \`y\` bound to \`m\` because \`$Required\` [1] is incompatible with undefined [2].
            References:
              9:     var y: $Required<Foo> = x;
                            ^^^^^^^^^^^^^^ [1]
             17:     function baz(n: null, m: void) {}
                                              ^^^^ [2]

          test.js:40
           40:     (pc: $Required<IPoint>); // error
                    ^^ Cannot cast \`pc\` to \`$Required\` because undefined [1] is incompatible with number [2] in property \`x\`.
            References:
             26:       x?: number;
                           ^^^^^^ [1]
             31:       x?: number,
                           ^^^^^^ [2]

          test.js:40
           40:     (pc: $Required<IPoint>); // error
                    ^^ Cannot cast \`pc\` to \`$Required\` because undefined [1] is incompatible with number [2] in property \`y\`.
            References:
             27:       y?: number;
                           ^^^^^^ [1]
             32:       y?: number
                           ^^^^^^ [2]

          test.js:43
           43:     (pc: $Required<{ x?: number }>); // error
                    ^^ Cannot cast \`pc\` to \`$Required\` because undefined [1] is incompatible with number [2] in property \`x\`.
            References:
             26:       x?: number;
                           ^^^^^^ [1]
             43:     (pc: $Required<{ x?: number }>); // error
                                          ^^^^^^ [2]

          test.js:45
           45:     (pi: PointC); // error
                    ^^ Cannot cast \`pi\` to \`PointC\` because \`IPoint\` [1] is incompatible with \`PointC\` [2].
            References:
             36:     declare var pi: IPoint;
                                     ^^^^^^ [1]
             45:     (pi: PointC); // error
                          ^^^^^^ [2]

          test.js:48
           48:     (pi: $Required<{ x?: number }>); // error
                    ^^ Cannot cast \`pi\` to \`$Required\` because undefined [1] is incompatible with number [2] in property \`x\`.
            References:
             31:       x?: number,
                           ^^^^^^ [1]
             48:     (pi: $Required<{ x?: number }>); // error
                                          ^^^^^^ [2]

          test.js:50
           50:     (ps: PointC); // error
                    ^^ Cannot cast \`ps\` to \`PointC\` because \`$Required\` [1] is incompatible with \`PointC\` [2].
            References:
             37:     declare var ps: $Required<IPoint>;
                                     ^^^^^^^^^^^^^^^^^ [1]
             50:     (ps: PointC); // error
                          ^^^^^^ [2]
        `,
      ),
    addCode(`
    declare class C {
      x?: number;
    }
    
    type A = {
      +x?: number
    };
    
    type B = {
      x?: number
    };
    
    declare var a_: A;
    declare var b_: B;
    declare var c_: C;
    
    (a_: $Required<A>);
    (a_: $Required<B>);
    (a_: $Required<C>);
    
    (b_: $Required<A>);
    (b_: $Required<B>);
    (b_: $Required<C>);
    
    (c_: $Required<A>);
    (c_: $Required<B>);
    (c_: $Required<C>);
    `).newErrors(
      `
          test.js:73
           73:     (a_: $Required<A>);
                    ^^ Cannot cast \`a_\` to \`$Required\` because undefined [1] is incompatible with number [1] in property \`x\`.
            References:
             62:       +x?: number
                            ^^^^^^ [1]

          test.js:74
           74:     (a_: $Required<B>);
                    ^^ Cannot cast \`a_\` to \`$Required\` because property \`x\` is read-only in \`A\` [1] but writable in \`$Required\` [2].
            References:
             69:     declare var a_: A;
                                     ^ [1]
             74:     (a_: $Required<B>);
                          ^^^^^^^^^^^^ [2]

          test.js:74
           74:     (a_: $Required<B>);
                    ^^ Cannot cast \`a_\` to \`$Required\` because undefined [1] is incompatible with number [2] in property \`x\`.
            References:
             62:       +x?: number
                            ^^^^^^ [1]
             66:       x?: number
                           ^^^^^^ [2]

          test.js:75
           75:     (a_: $Required<C>);
                    ^^ Cannot cast \`a_\` to \`$Required\` because property \`x\` is read-only in \`A\` [1] but writable in \`$Required\` [2].
            References:
             69:     declare var a_: A;
                                     ^ [1]
             75:     (a_: $Required<C>);
                          ^^^^^^^^^^^^ [2]

          test.js:77
           77:     (b_: $Required<A>);
                    ^^ Cannot cast \`b_\` to \`$Required\` because undefined [1] is incompatible with number [2] in property \`x\`.
            References:
             66:       x?: number
                           ^^^^^^ [1]
             62:       +x?: number
                            ^^^^^^ [2]

          test.js:78
           78:     (b_: $Required<B>);
                    ^^ Cannot cast \`b_\` to \`$Required\` because undefined [1] is incompatible with number [1] in property \`x\`.
            References:
             66:       x?: number
                           ^^^^^^ [1]

          test.js:81
           81:     (c_: $Required<A>);
                    ^^ Cannot cast \`c_\` to \`$Required\` because undefined [1] is incompatible with number [2] in property \`x\`.
            References:
             58:       x?: number;
                           ^^^^^^ [1]
             62:       +x?: number
                            ^^^^^^ [2]

          test.js:82
           82:     (c_: $Required<B>);
                    ^^ Cannot cast \`c_\` to \`$Required\` because undefined [1] is incompatible with number [2] in property \`x\`.
            References:
             58:       x?: number;
                           ^^^^^^ [1]
             66:       x?: number
                           ^^^^^^ [2]
        `
    ),
    addCode(`
    declare class One {
      foo: number;
      constructor(x: number): void;
    }
    
    declare class Two {
      foo: number;
      constructor(): void;
    }
    
    type Three = { foo?: number, constructor(): () => void };
    
    let one = new One(3);
    let two = new Two();
    
    (one: One);
    (one: Two); // error
    (one: Three); // error
    (one: $Required<One>);
    (one: $Required<Two>);
    (one: $Required<Three>); // error
    
    (two: One); // error
    (two: Two);
    (two: Three); // error
    (two: $Required<One>); // error
    (two: $Required<Two>);
    (two: $Required<Three>); // error
    `).newErrors(
      `
          test.js:103
          103:     (one: Two); // error
                    ^^^ Cannot cast \`one\` to \`Two\` because \`One\` [1] is incompatible with \`Two\` [2].
            References:
             99:     let one = new One(3);
                               ^^^^^^^^^^ [1]
            103:     (one: Two); // error
                           ^^^ [2]

          test.js:104
          104:     (one: Three); // error
                    ^^^ Cannot cast \`one\` to \`Three\` because function type [1] requires another argument from function type [2] in property \`constructor\`.
            References:
             89:       constructor(x: number): void;
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ [1]
             97:     type Three = { foo?: number, constructor(): () => void };
                                                  ^^^^^^^^^^^^^^^^^^^^^^^^^ [2]

          test.js:104
          104:     (one: Three); // error
                    ^^^ Cannot cast \`one\` to \`Three\` because undefined [1] is incompatible with function type [2] in the return value of property \`constructor\`.
            References:
             89:       constructor(x: number): void;
                                               ^^^^ [1]
             97:     type Three = { foo?: number, constructor(): () => void };
                                                                 ^^^^^^^^^^ [2]

          test.js:104
          104:     (one: Three); // error
                    ^^^ Cannot cast \`one\` to \`Three\` because number [1] is incompatible with undefined [2] in property \`foo\`.
            References:
             88:       foo: number;
                            ^^^^^^ [1]
             97:     type Three = { foo?: number, constructor(): () => void };
                                          ^^^^^^ [2]

          test.js:107
          107:     (one: $Required<Three>); // error
                    ^^^ Cannot cast \`one\` to \`$Required\` because function type [1] requires another argument from function type [2] in property \`constructor\`.
            References:
             89:       constructor(x: number): void;
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ [1]
             97:     type Three = { foo?: number, constructor(): () => void };
                                                  ^^^^^^^^^^^^^^^^^^^^^^^^^ [2]

          test.js:107
          107:     (one: $Required<Three>); // error
                    ^^^ Cannot cast \`one\` to \`$Required\` because undefined [1] is incompatible with function type [2] in the return value of property \`constructor\`.
            References:
             89:       constructor(x: number): void;
                                               ^^^^ [1]
             97:     type Three = { foo?: number, constructor(): () => void };
                                                                 ^^^^^^^^^^ [2]

          test.js:109
          109:     (two: One); // error
                    ^^^ Cannot cast \`two\` to \`One\` because \`Two\` [1] is incompatible with \`One\` [2].
            References:
            100:     let two = new Two();
                               ^^^^^^^^^ [1]
            109:     (two: One); // error
                           ^^^ [2]

          test.js:111
          111:     (two: Three); // error
                    ^^^ Cannot cast \`two\` to \`Three\` because undefined [1] is incompatible with function type [2] in the return value of property \`constructor\`.
            References:
             94:       constructor(): void;
                                      ^^^^ [1]
             97:     type Three = { foo?: number, constructor(): () => void };
                                                                 ^^^^^^^^^^ [2]

          test.js:111
          111:     (two: Three); // error
                    ^^^ Cannot cast \`two\` to \`Three\` because number [1] is incompatible with undefined [2] in property \`foo\`.
            References:
             93:       foo: number;
                            ^^^^^^ [1]
             97:     type Three = { foo?: number, constructor(): () => void };
                                          ^^^^^^ [2]

          test.js:114
          114:     (two: $Required<Three>); // error
                    ^^^ Cannot cast \`two\` to \`$Required\` because undefined [1] is incompatible with function type [2] in the return value of property \`constructor\`.
            References:
             94:       constructor(): void;
                                      ^^^^ [1]
             97:     type Three = { foo?: number, constructor(): () => void };
                                                                 ^^^^^^^^^^ [2]
        `
    )
  ])
]);
