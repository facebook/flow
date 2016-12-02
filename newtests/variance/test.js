/* @flow */

import {suite, test} from '../../tsrc/test/Tester';

export default suite(({addFile, addFiles, addCode}) => [
  test('X ~> A', [
    // literal A
    addCode('({p: new A}: RWA);').noNewErrors(),
    addCode('({p: new A}: dRWA);').noNewErrors(),

    // A
    addCode('(rwA: RWA);').noNewErrors(),
    addCode('(drwA: dRWA);').noNewErrors(),

    // +A
    addCode('(roA: RWA);')
      .newErrors(
        `
          test.js:50
           50: (roA: RWA);
                ^^^ object type. Covariant property \`p\` incompatible with invariant use in
           50: (roA: RWA);
                     ^^^ object type
        `,
      ),
    addCode('(droA: dRWA);')
      .newErrors(
        `
          test.js:52
           52: (droA: dRWA);
                ^^^^ object type. Covariant computed property incompatible with invariant use in
           52: (droA: dRWA);
                      ^^^^ object type
        `,
      ),

    // -A
    addCode('(woA: RWA);')
      .newErrors(
        `
          test.js:54
           54: (woA: RWA);
                ^^^ object type. Contravariant property \`p\` incompatible with invariant use in
           54: (woA: RWA);
                     ^^^ object type
        `,
      ),
    addCode('(dwoA: dRWA);')
      .newErrors(
        `
          test.js:56
           56: (dwoA: dRWA);
                ^^^^ object type. Contravariant computed property incompatible with invariant use in
           56: (dwoA: dRWA);
                      ^^^^ object type
        `,
      ),


    // literal B
    addCode('({p: new B}: RWA);').noNewErrors(),
    addCode('({p: new B}: dRWA);').noNewErrors(),

    // B
    addCode('(rwB: RWA);')
      .newErrors(
        `
          test.js:9
            9:     type RWA = {p: A}
                                  ^ A. This type is incompatible with
           12:     type RWB = {p: B}
                                  ^ B
        `,
      ),
    addCode('(drwB: dRWA);')
      .newErrors(
        `
          test.js:26
           26:     type dRWA = {[string]: A};
                                          ^ A. This type is incompatible with
           29:     type dRWB = {[string]: B};
                                          ^ B
        `,
      ),

    // +B
    addCode('(roB: RWA);')
      .newErrors(
        `
          test.js:66
           66: (roB: RWA);
                ^^^ object type. Covariant property \`p\` incompatible with invariant use in
           66: (roB: RWA);
                     ^^^ object type
        `,
      ),
    addCode('(droB: dRWA);')
      .newErrors(
        `
          test.js:68
           68: (droB: dRWA);
                ^^^^ object type. Covariant computed property incompatible with invariant use in
           68: (droB: dRWA);
                      ^^^^ object type
        `,
      ),

    // -B
    addCode('(woB: RWA);')
      .newErrors(
        `
          test.js:9
            9:     type RWA = {p: A}
                                  ^ A. This type is incompatible with
           14:     type WOB = {-p: B}
                                   ^ B

          test.js:70
           70: (woB: RWA);
                ^^^ object type. Contravariant property \`p\` incompatible with invariant use in
           70: (woB: RWA);
                     ^^^ object type
        `,
      ),
    addCode('(dwoB: dRWA);')
      .newErrors(
        `
          test.js:26
           26:     type dRWA = {[string]: A};
                                          ^ A. This type is incompatible with
           31:     type dWOB = {-[string]: B};
                                           ^ B

          test.js:72
           72: (dwoB: dRWA);
                ^^^^ object type. Contravariant computed property incompatible with invariant use in
           72: (dwoB: dRWA);
                      ^^^^ object type
        `,
      ),
  ]),

  test('X ~> +A', [
    // literal A
    addCode('({p: new A}: ROA);').noNewErrors(),
    addCode('({p: new A}: dROA);').noNewErrors(),

    // A
    addCode('(rwA: ROA);').noNewErrors(),
    addCode('(drwA: dROA);').noNewErrors(),

    // +A
    addCode('(roA: ROA);').noNewErrors(),
    addCode('(droA: dROA);').noNewErrors(),

    // -A
    addCode('(woA: ROA);')
      .newErrors(
        `
          test.js:54
           54: (woA: ROA);
                ^^^ object type. Contravariant property \`p\` incompatible with covariant use in
           54: (woA: ROA);
                     ^^^ object type
        `,
      ),
    addCode('(dwoA: dROA);')
      .newErrors(
        `
          test.js:56
           56: (dwoA: dROA);
                ^^^^ object type. Contravariant computed property incompatible with covariant use in
           56: (dwoA: dROA);
                      ^^^^ object type
        `,
      ),

    // literal B
    addCode('({p: new B}: ROA);').noNewErrors(),
    addCode('({p: new B}: dROA);').noNewErrors(),

    // B
    addCode('(rwB: ROA);').noNewErrors(),
    addCode('(drwB: dROA);').noNewErrors(),

    // +B
    addCode('(roB: ROA);').noNewErrors(),
    addCode('(droB: dROA);').noNewErrors(),

    // -B
    addCode('(woB: ROA);')
      .newErrors(
        `
          test.js:70
           70: (woB: ROA);
                ^^^ object type. Contravariant property \`p\` incompatible with covariant use in
           70: (woB: ROA);
                     ^^^ object type
        `,
      ),
    addCode('(dwoB: dROA);')
      .newErrors(
        `
          test.js:72
           72: (dwoB: dROA);
                ^^^^ object type. Contravariant computed property incompatible with covariant use in
           72: (dwoB: dROA);
                      ^^^^ object type
        `,
      ),
  ]),

  test('X ~> -A', [
    // literal A
    addCode('({p: new A}: WOA);').noNewErrors(),
    addCode('({p: new A}: dWOA);').noNewErrors(),

    // A
    addCode('(rwA: WOA);').noNewErrors(),
    addCode('(rwA: dWOA);').noNewErrors(),

    // +A
    addCode('(roA: WOA);')
      .newErrors(
        `
          test.js:50
           50: (roA: WOA);
                ^^^ object type. Covariant property \`p\` incompatible with contravariant use in
           50: (roA: WOA);
                     ^^^ object type
        `,
      ),
    addCode('(droA: dWOA);')
      .newErrors(
        `
          test.js:52
           52: (droA: dWOA);
                ^^^^ object type. Covariant computed property incompatible with contravariant use in
           52: (droA: dWOA);
                      ^^^^ object type
        `,
      ),

    // -A
    addCode('(woA: WOA);').noNewErrors(),
    addCode('(dwoA: dWOA);').noNewErrors(),

    // literal B
    addCode('({p: new B}: WOA);').noNewErrors(),
    addCode('({p: new B}: dWOA);').noNewErrors(),

    // B
    addCode('(rwB: WOA);')
      .newErrors(
        `
          test.js:11
           11:     type WOA = {-p: A}
                                   ^ A. This type is incompatible with
           12:     type RWB = {p: B}
                                  ^ B
        `,
      ),
    addCode('(drwB: dWOA);')
      .newErrors(
        `
          test.js:28
           28:     type dWOA = {-[string]: A};
                                           ^ A. This type is incompatible with
           29:     type dRWB = {[string]: B};
                                          ^ B
        `,
      ),

    // +B
    addCode('(roB: WOA);')
      .newErrors(
        `
          test.js:66
           66: (roB: WOA);
                ^^^ object type. Covariant property \`p\` incompatible with contravariant use in
           66: (roB: WOA);
                     ^^^ object type
        `,
      ),
    addCode('(droB: dWOA);')
      .newErrors(
        `
          test.js:68
           68: (droB: dWOA);
                ^^^^ object type. Covariant computed property incompatible with contravariant use in
           68: (droB: dWOA);
                      ^^^^ object type
        `,
      ),

    // -B
    addCode('(woB: WOA);')
      .newErrors(
        `
          test.js:11
           11:     type WOA = {-p: A}
                                   ^ A. This type is incompatible with
           14:     type WOB = {-p: B}
                                   ^ B
        `,
      ),
    addCode('(dwoB: dWOA);')
      .newErrors(
        `
          test.js:28
           28:     type dWOA = {-[string]: A};
                                           ^ A. This type is incompatible with
           31:     type dWOB = {-[string]: B};
                                           ^ B
        `,
      ),
  ]),

  test('X ~> B', [
    // literal A
    addCode('({p: new A}: RWB);')
      .newErrors(
        `
          test.js:42
           42: ({p: new A}: RWB);
                    ^^^^^ A. This type is incompatible with
           12:     type RWB = {p: B}
                                  ^ B
        `,
      ),
    addCode('({p: new A}: dRWB);')
      .newErrors(
        `
          test.js:44
           44: ({p: new A}: dRWB);
                    ^^^^^ A. This type is incompatible with
           29:     type dRWB = {[string]: B};
                                          ^ B
        `,
      ),

    // A
    addCode('(rwA: RWB);')
      .newErrors(
        `
          test.js:9
            9:     type RWA = {p: A}
                                  ^ A. This type is incompatible with
           12:     type RWB = {p: B}
                                  ^ B
        `,
      ),
    addCode('(drwA: dRWB);')
      .newErrors(
        `
          test.js:26
           26:     type dRWA = {[string]: A};
                                          ^ A. This type is incompatible with
           29:     type dRWB = {[string]: B};
                                          ^ B
        `,
      ),

    // +A
    addCode('(roA: RWB);')
      .newErrors(
        `
          test.js:10
           10:     type ROA = {+p: A}
                                   ^ A. This type is incompatible with
           12:     type RWB = {p: B}
                                  ^ B

          test.js:50
           50: (roA: RWB);
                ^^^ object type. Covariant property \`p\` incompatible with invariant use in
           50: (roA: RWB);
                     ^^^ object type
        `,
      ),
    addCode('(droA: dRWB);')
      .newErrors(
        `
          test.js:27
           27:     type dROA = {+[string]: A};
                                           ^ A. This type is incompatible with
           29:     type dRWB = {[string]: B};
                                          ^ B

          test.js:52
           52: (droA: dRWB);
                ^^^^ object type. Covariant computed property incompatible with invariant use in
           52: (droA: dRWB);
                      ^^^^ object type
        `,
      ),

    // -A
    addCode('(woA: RWB);')
      .newErrors(
        `
          test.js:54
           54: (woA: RWB);
                ^^^ object type. Contravariant property \`p\` incompatible with invariant use in
           54: (woA: RWB);
                     ^^^ object type
        `,
      ),
    addCode('(dwoA: dRWB);')
      .newErrors(
        `
          test.js:56
           56: (dwoA: dRWB);
                ^^^^ object type. Contravariant computed property incompatible with invariant use in
           56: (dwoA: dRWB);
                      ^^^^ object type
        `,
      ),
  ]),

  test('X ~> +B', [
    // literal A
    addCode('({p: new A}: ROB);')
      .newErrors(
        `
          test.js:42
           42: ({p: new A}: ROB);
                    ^^^^^ A. This type is incompatible with
           13:     type ROB = {+p: B}
                                   ^ B
        `,
      ),
    addCode('({p: new A}: dROB);')
      .newErrors(
        `
          test.js:44
           44: ({p: new A}: dROB);
                    ^^^^^ A. This type is incompatible with
           30:     type dROB = {+[string]: B};
                                           ^ B
        `,
      ),

    // A
    addCode('(rwA: ROB);')
      .newErrors(
        `
          test.js:9
            9:     type RWA = {p: A}
                                  ^ A. This type is incompatible with
           13:     type ROB = {+p: B}
                                   ^ B
        `,
      ),
    addCode('(drwA: dROB);')
      .newErrors(
        `
          test.js:26
           26:     type dRWA = {[string]: A};
                                          ^ A. This type is incompatible with
           30:     type dROB = {+[string]: B};
                                           ^ B
        `,
      ),

    // +A
    addCode('(roA: ROB);')
      .newErrors(
        `
          test.js:10
           10:     type ROA = {+p: A}
                                   ^ A. This type is incompatible with
           13:     type ROB = {+p: B}
                                   ^ B
        `,
      ),
    addCode('(droA: dROB);')
      .newErrors(
        `
          test.js:27
           27:     type dROA = {+[string]: A};
                                           ^ A. This type is incompatible with
           30:     type dROB = {+[string]: B};
                                           ^ B
        `,
      ),

    // -A
    addCode('(woA: ROB);')
      .newErrors(
        `
          test.js:54
           54: (woA: ROB);
                ^^^ object type. Contravariant property \`p\` incompatible with covariant use in
           54: (woA: ROB);
                     ^^^ object type
        `,
      ),
    addCode('(dwoA: dROB);')
      .newErrors(
        `
          test.js:56
           56: (dwoA: dROB);
                ^^^^ object type. Contravariant computed property incompatible with covariant use in
           56: (dwoA: dROB);
                      ^^^^ object type
        `,
      ),
  ]),

  test('X ~> -B', [
    // literal A
    addCode('({p: new A}: WOB);').noNewErrors(),
    addCode('({p: new A}: dWOB);').noNewErrors(),

    // A
    addCode('(rwA: WOB);').noNewErrors(),
    addCode('(drwA: dWOB);').noNewErrors(),

    // +A
    addCode('(roA: WOB);')
      .newErrors(
        `
          test.js:50
           50: (roA: WOB);
                ^^^ object type. Covariant property \`p\` incompatible with contravariant use in
           50: (roA: WOB);
                     ^^^ object type
        `,
      ),
    addCode('(droA: dWOB);')
      .newErrors(
        `
          test.js:52
           52: (droA: dWOB);
                ^^^^ object type. Covariant computed property incompatible with contravariant use in
           52: (droA: dWOB);
                      ^^^^ object type
        `,
      ),

    // -A
    addCode('(woA: WOB);').noNewErrors(),
    addCode('(dwoA: dWOB);').noNewErrors(),
  ]),

  test('unification', [
    // Note: these tests don't reuse the type aliases from the prelude because
    // doing so results in "naive" unification instead of rec_unify.

    addCode('(([rwA]: Array<{p:A}>): Array<{p:A}>);')
      .noNewErrors(),

    addCode('(([roA]: Array<{+p:A}>): Array<{p:A}>);')
      .newErrors(
        `
          test.js:44
           44: (([roA]: Array<{+p:A}>): Array<{p:A}>);
                              ^^^^^^ object type. Covariant property \`p\` incompatible with invariant use in
           44: (([roA]: Array<{+p:A}>): Array<{p:A}>);
                                              ^^^^^ object type
        `,
      ),

    addCode('(([woA]: Array<{-p:A}>): Array<{p:A}>);')
      .newErrors(
        `
          test.js:46
           46: (([woA]: Array<{-p:A}>): Array<{p:A}>);
                              ^^^^^^ object type. Contravariant property \`p\` incompatible with invariant use in
           46: (([woA]: Array<{-p:A}>): Array<{p:A}>);
                                              ^^^^^ object type
        `,
      ),

    addCode('(([rwA]: Array<{p:A}>): Array<{+p:A}>);')
      .newErrors(
        `
          test.js:48
           48: (([rwA]: Array<{p:A}>): Array<{+p:A}>);
                              ^^^^^ object type. Invariant property \`p\` incompatible with covariant use in
           48: (([rwA]: Array<{p:A}>): Array<{+p:A}>);
                                             ^^^^^^ object type
        `,
      ),

    addCode('(([roA]: Array<{+p:A}>): Array<{+p:A}>);')
      .noNewErrors(),

    addCode('(([woA]: Array<{-p:A}>): Array<{+p:A}>);')
      .newErrors(
        `
          test.js:52
           52: (([woA]: Array<{-p:A}>): Array<{+p:A}>);
                              ^^^^^^ object type. Contravariant property \`p\` incompatible with covariant use in
           52: (([woA]: Array<{-p:A}>): Array<{+p:A}>);
                                              ^^^^^^ object type
        `,
      ),

    addCode('(([rwA]: Array<{p:A}>): Array<{-p:A}>);')
      .newErrors(
        `
          test.js:54
           54: (([rwA]: Array<{p:A}>): Array<{-p:A}>);
                              ^^^^^ object type. Invariant property \`p\` incompatible with contravariant use in
           54: (([rwA]: Array<{p:A}>): Array<{-p:A}>);
                                             ^^^^^^ object type
        `,
      ),

    addCode('(([roA]: Array<{+p:A}>): Array<{-p:A}>);')
      .newErrors(
        `
          test.js:56
           56: (([roA]: Array<{+p:A}>): Array<{-p:A}>);
                              ^^^^^^ object type. Covariant property \`p\` incompatible with contravariant use in
           56: (([roA]: Array<{+p:A}>): Array<{-p:A}>);
                                              ^^^^^^ object type
        `,
      ),

    addCode('(([woA]: Array<{-p:A}>): Array<{-p:A}>);')
      .noNewErrors(),
  ]),

  test('$Shape', [
    // A ~> A
    addCode('(rwA: $Shape<RWA>);').noNewErrors(),

    // +A ~> A
    addCode('(roA: $Shape<RWA>);').noNewErrors(),

    // -A ~> A
    addCode('(woA: $Shape<RWA>);').
      newErrors(
       `
         test.js:46
          46: (woA: \$Shape<RWA>);
               ^^^ object type. Contravariant property \`p\` incompatible with covariant use in
          46: (woA: \$Shape<RWA>);
                           ^^^ RWA
       `,
     ),

    // B ~> A
    addCode('(rwB: $Shape<RWA>);').noNewErrors(),

    // +B ~> A
    addCode('(roB: $Shape<RWA>);').noNewErrors(),

    // -B ~> A
    addCode('(woB: $Shape<RWA>);').
      newErrors(
       `
         test.js:52
          52: (woB: \$Shape<RWA>);
               ^^^ object type. Contravariant property \`p\` incompatible with covariant use in
          52: (woB: \$Shape<RWA>);
                           ^^^ RWA
       `,
     ),

    // A ~> B
    addCode('(rwA: $Shape<RWB>);').
      newErrors(
       `
         test.js:9
           9:     type RWA = {p: A}
                                 ^ A. This type is incompatible with
          12:     type RWB = {p: B}
                                 ^ B
       `,
     ),

    // +A ~> B
    addCode('(roA: $Shape<RWB>);').
      newErrors(
       `
         test.js:10
          10:     type ROA = {+p: A}
                                  ^ A. This type is incompatible with
          12:     type RWB = {p: B}
                                 ^ B
       `,
     ),

    // -A ~> B
    addCode('(woA: $Shape<RWB>);').
      newErrors(
       `
         test.js:58
          58: (woA: \$Shape<RWB>);
               ^^^ object type. Contravariant property \`p\` incompatible with covariant use in
          58: (woA: \$Shape<RWB>);
                           ^^^ RWB
       `,
     ),
  ]),

]).beforeEach(({addCode}) => [
  addCode(`
    class A {}
    class B extends A {};

    // named properties

    type RWA = {p: A}
    type ROA = {+p: A}
    type WOA = {-p: A}
    type RWB = {p: B}
    type ROB = {+p: B}
    type WOB = {-p: B}

    declare var rwA: RWA;
    declare var roA: ROA;
    declare var woA: WOA;

    declare var rwB: RWB;
    declare var roB: ROB;
    declare var woB: WOB;

    // dictionaries

    type dRWA = {[string]: A};
    type dROA = {+[string]: A};
    type dWOA = {-[string]: A};
    type dRWB = {[string]: B};
    type dROB = {+[string]: B};
    type dWOB = {-[string]: B};

    declare var drwA: dRWA;
    declare var droA: dROA;
    declare var dwoA: dWOA;

    declare var drwB: dRWB;
    declare var droB: dROB;
    declare var dwoB: dWOB;
  `),
]);
