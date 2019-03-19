/*
 * @flow
 * @lint-ignore-every LINEWRAP1
 */

import { suite, test } from "flow-dev-tools/src/test/Tester";

export default suite(({ addFile, addFiles, addCode }) => [
  test("$Required<T>", [
    addFile("polymorphic.js").newErrors(
                               `
                                 polymorphic.js:3
                                   3: foo({ a: undefined }); // error
                                               ^^^^^^^^^ Cannot call \`foo\` with object literal bound to \`val\` because undefined [1] is incompatible with number [2] in property \`a\`.
                                   References:
                                    13: declare var undefined: void;
                                                               ^^^^ [1]. See lib: [LIB] core.js:13
                                     1: declare function foo<T: $Required<{ a?: number }>>(val: T): void;
                                                                                ^^^^^^ [2]

                                 polymorphic.js:8
                                   8: bar({ a: undefined }); // error
                                               ^^^^^^^^^ Cannot call \`bar\` with object literal bound to \`val\` because undefined [1] is incompatible with number [2] in property \`a\`.
                                   References:
                                    13: declare var undefined: void;
                                                               ^^^^ [1]. See lib: [LIB] core.js:13
                                     6: declare function bar<T: { a?: number }>(val: $Required<T>): void;
                                                                      ^^^^^^ [2]
                               `,
                             ),
    addFile("variance.js").newErrors(
      `
                              variance.js:9
                                9: foo.readonly = undefined; // error
                                       ^^^^^^^^ Cannot assign \`undefined\` to \`foo.readonly\` because property \`readonly\` is not writable.

                              variance.js:10
                               10: foo.readonly = 1; // error
                                       ^^^^^^^^ Cannot assign \`1\` to \`foo.readonly\` because property \`readonly\` is not writable.

                              variance.js:13
                               13: foo.writeonly = undefined; // error
                                                   ^^^^^^^^^ Cannot assign \`undefined\` to \`foo.writeonly\` because undefined [1] is incompatible with string [2].
                                References:
                                 13: declare var undefined: void;
                                                            ^^^^ [1]. See lib: [LIB] core.js:13
                                  5:   -writeonly?: string,
                                                    ^^^^^^ [2]

                              variance.js:17
                               17: foo.neutral = undefined; // error
                                                 ^^^^^^^^^ Cannot assign \`undefined\` to \`foo.neutral\` because undefined [1] is incompatible with number [2].
                                References:
                                 13: declare var undefined: void;
                                                            ^^^^ [1]. See lib: [LIB] core.js:13
                                  6:   neutral?: number
                                                 ^^^^^^ [2]
                            `
    ),
    addFile("unions.js").noNewErrors(),
    addFile("disjoint_unions.js").noNewErrors(),
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
        `
    ),
    addCode(`
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
          test.js:42
           42:     (pc: $Required<IPoint>); // error
                    ^^ Cannot cast \`pc\` to \`$Required\` because undefined [1] is incompatible with number [2] in property \`x\`.
            References:
             28:       x?: number;
                           ^^^^^^ [1]
             33:       x?: number,
                           ^^^^^^ [2]

          test.js:42
           42:     (pc: $Required<IPoint>); // error
                    ^^ Cannot cast \`pc\` to \`$Required\` because undefined [1] is incompatible with number [2] in property \`y\`.
            References:
             29:       y?: number;
                           ^^^^^^ [1]
             34:       y?: number
                           ^^^^^^ [2]

          test.js:45
           45:     (pc: $Required<{ x?: number }>); // error
                    ^^ Cannot cast \`pc\` to \`$Required\` because undefined [1] is incompatible with number [2] in property \`x\`.
            References:
             28:       x?: number;
                           ^^^^^^ [1]
             45:     (pc: $Required<{ x?: number }>); // error
                                          ^^^^^^ [2]

          test.js:47
           47:     (pi: PointC); // error
                    ^^ Cannot cast \`pi\` to \`PointC\` because \`IPoint\` [1] is incompatible with \`PointC\` [2].
            References:
             38:     declare var pi: IPoint;
                                     ^^^^^^ [1]
             47:     (pi: PointC); // error
                          ^^^^^^ [2]

          test.js:50
           50:     (pi: $Required<{ x?: number }>); // error
                    ^^ Cannot cast \`pi\` to \`$Required\` because undefined [1] is incompatible with number [2] in property \`x\`.
            References:
             33:       x?: number,
                           ^^^^^^ [1]
             50:     (pi: $Required<{ x?: number }>); // error
                                          ^^^^^^ [2]

          test.js:52
           52:     (ps: PointC); // error
                    ^^ Cannot cast \`ps\` to \`PointC\` because \`$Required\` [1] is incompatible with \`PointC\` [2].
            References:
             39:     declare var ps: $Required<IPoint>;
                                     ^^^^^^^^^^^^^^^^^ [1]
             52:     (ps: PointC); // error
                          ^^^^^^ [2]
        `
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
          test.js:75
           75:     (a_: $Required<A>);
                    ^^ Cannot cast \`a_\` to \`$Required\` because undefined [1] is incompatible with number [1] in property \`x\`.
            References:
             64:       +x?: number
                            ^^^^^^ [1]

          test.js:76
           76:     (a_: $Required<B>);
                    ^^ Cannot cast \`a_\` to \`$Required\` because property \`x\` is read-only in \`A\` [1] but writable in \`$Required\` [2].
            References:
             71:     declare var a_: A;
                                     ^ [1]
             76:     (a_: $Required<B>);
                          ^^^^^^^^^^^^ [2]

          test.js:76
           76:     (a_: $Required<B>);
                    ^^ Cannot cast \`a_\` to \`$Required\` because undefined [1] is incompatible with number [2] in property \`x\`.
            References:
             64:       +x?: number
                            ^^^^^^ [1]
             68:       x?: number
                           ^^^^^^ [2]

          test.js:77
           77:     (a_: $Required<C>);
                    ^^ Cannot cast \`a_\` to \`$Required\` because property \`x\` is read-only in \`A\` [1] but writable in \`$Required\` [2].
            References:
             71:     declare var a_: A;
                                     ^ [1]
             77:     (a_: $Required<C>);
                          ^^^^^^^^^^^^ [2]

          test.js:79
           79:     (b_: $Required<A>);
                    ^^ Cannot cast \`b_\` to \`$Required\` because undefined [1] is incompatible with number [2] in property \`x\`.
            References:
             68:       x?: number
                           ^^^^^^ [1]
             64:       +x?: number
                            ^^^^^^ [2]

          test.js:80
           80:     (b_: $Required<B>);
                    ^^ Cannot cast \`b_\` to \`$Required\` because undefined [1] is incompatible with number [1] in property \`x\`.
            References:
             68:       x?: number
                           ^^^^^^ [1]

          test.js:83
           83:     (c_: $Required<A>);
                    ^^ Cannot cast \`c_\` to \`$Required\` because undefined [1] is incompatible with number [2] in property \`x\`.
            References:
             60:       x?: number;
                           ^^^^^^ [1]
             64:       +x?: number
                            ^^^^^^ [2]

          test.js:84
           84:     (c_: $Required<B>);
                    ^^ Cannot cast \`c_\` to \`$Required\` because undefined [1] is incompatible with number [2] in property \`x\`.
            References:
             60:       x?: number;
                           ^^^^^^ [1]
             68:       x?: number
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
          test.js:105
          105:     (one: Two); // error
                    ^^^ Cannot cast \`one\` to \`Two\` because \`One\` [1] is incompatible with \`Two\` [2].
            References:
            101:     let one = new One(3);
                               ^^^^^^^^^^ [1]
            105:     (one: Two); // error
                           ^^^ [2]

          test.js:106
          106:     (one: Three); // error
                    ^^^ Cannot cast \`one\` to \`Three\` because function type [1] requires another argument from function type [2] in property \`constructor\`.
            References:
             91:       constructor(x: number): void;
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ [1]
             99:     type Three = { foo?: number, constructor(): () => void };
                                                  ^^^^^^^^^^^^^^^^^^^^^^^^^ [2]

          test.js:106
          106:     (one: Three); // error
                    ^^^ Cannot cast \`one\` to \`Three\` because undefined [1] is incompatible with function type [2] in the return value of property \`constructor\`.
            References:
             91:       constructor(x: number): void;
                                               ^^^^ [1]
             99:     type Three = { foo?: number, constructor(): () => void };
                                                                 ^^^^^^^^^^ [2]

          test.js:106
          106:     (one: Three); // error
                    ^^^ Cannot cast \`one\` to \`Three\` because number [1] is incompatible with undefined [2] in property \`foo\`.
            References:
             90:       foo: number;
                            ^^^^^^ [1]
             99:     type Three = { foo?: number, constructor(): () => void };
                                          ^^^^^^ [2]

          test.js:109
          109:     (one: $Required<Three>); // error
                    ^^^ Cannot cast \`one\` to \`$Required\` because function type [1] requires another argument from function type [2] in property \`constructor\`.
            References:
             91:       constructor(x: number): void;
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ [1]
             99:     type Three = { foo?: number, constructor(): () => void };
                                                  ^^^^^^^^^^^^^^^^^^^^^^^^^ [2]

          test.js:109
          109:     (one: $Required<Three>); // error
                    ^^^ Cannot cast \`one\` to \`$Required\` because undefined [1] is incompatible with function type [2] in the return value of property \`constructor\`.
            References:
             91:       constructor(x: number): void;
                                               ^^^^ [1]
             99:     type Three = { foo?: number, constructor(): () => void };
                                                                 ^^^^^^^^^^ [2]

          test.js:111
          111:     (two: One); // error
                    ^^^ Cannot cast \`two\` to \`One\` because \`Two\` [1] is incompatible with \`One\` [2].
            References:
            102:     let two = new Two();
                               ^^^^^^^^^ [1]
            111:     (two: One); // error
                           ^^^ [2]

          test.js:113
          113:     (two: Three); // error
                    ^^^ Cannot cast \`two\` to \`Three\` because undefined [1] is incompatible with function type [2] in the return value of property \`constructor\`.
            References:
             96:       constructor(): void;
                                      ^^^^ [1]
             99:     type Three = { foo?: number, constructor(): () => void };
                                                                 ^^^^^^^^^^ [2]

          test.js:113
          113:     (two: Three); // error
                    ^^^ Cannot cast \`two\` to \`Three\` because number [1] is incompatible with undefined [2] in property \`foo\`.
            References:
             95:       foo: number;
                            ^^^^^^ [1]
             99:     type Three = { foo?: number, constructor(): () => void };
                                          ^^^^^^ [2]

          test.js:116
          116:     (two: $Required<Three>); // error
                    ^^^ Cannot cast \`two\` to \`$Required\` because undefined [1] is incompatible with function type [2] in the return value of property \`constructor\`.
            References:
             96:       constructor(): void;
                                      ^^^^ [1]
             99:     type Three = { foo?: number, constructor(): () => void };
                                                                 ^^^^^^^^^^ [2]
        `
    )
  ])
]);
