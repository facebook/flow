/* @flow */

import {suite, test} from '../../tsrc/test/Tester';

export default suite(({addFile, addFiles, addCode}) => [
  test('X ~> A', [
    // literal A
    addCode('({p: new A}: RWA);').noNewErrors(),

    // A
    addCode('(rwA: RWA);').noNewErrors(),

    // +A
    addCode('(roA: RWA);')
      .newErrors(
        `
          test.js:27
           27: (roA: RWA);
                ^^^ object type. Covariant property \`p\` incompatible with invariant use in
           27: (roA: RWA);
                     ^^^ object type
        `,
      ),

    // -A
    addCode('(woA: RWA);')
      .newErrors(
        `
          test.js:29
           29: (woA: RWA);
                ^^^ object type. Contravariant property \`p\` incompatible with invariant use in
           29: (woA: RWA);
                     ^^^ object type
        `,
      ),

    // literal B
    addCode('({p: new B}: RWA);').noNewErrors(),

    // B
    addCode('(rwB: RWA);')
      .newErrors(
        `
          test.js:7
            7:     type RWA = {p: A}
                                  ^ A. This type is incompatible with
           10:     type RWB = {p: B}
                                  ^ B
        `,
      ),

    // +B
    addCode('(roB: RWA);')
      .newErrors(
        `
          test.js:35
           35: (roB: RWA);
                ^^^ object type. Covariant property \`p\` incompatible with invariant use in
           35: (roB: RWA);
                     ^^^ object type
        `,
      ),

    // -B
    addCode('(woB: RWA);')
      .newErrors(
        `
          test.js:7
            7:     type RWA = {p: A}
                                  ^ A. This type is incompatible with
           12:     type WOB = {-p: B}
                                   ^ B

          test.js:37
           37: (woB: RWA);
                ^^^ object type. Contravariant property \`p\` incompatible with invariant use in
           37: (woB: RWA);
                     ^^^ object type
        `,
      ),
  ]),

  test('X ~> +A', [
    // literal A
    addCode('({p: new A}: ROA);').noNewErrors(),

    // A
    addCode('(rwA: ROA);').noNewErrors(),

    // +A
    addCode('(roA: ROA);').noNewErrors(),

    // -A
    addCode('(woA: ROA);')
      .newErrors(
        `
          test.js:29
           29: (woA: ROA);
                ^^^ object type. Contravariant property \`p\` incompatible with covariant use in
           29: (woA: ROA);
                     ^^^ object type
        `,
      ),

    // literal B
    addCode('({p: new B}: ROA);').noNewErrors(),

    // B
    addCode('(rwB: ROA);').noNewErrors(),

    // +B
    addCode('(roB: ROA);').noNewErrors(),

    // -B
    addCode('(woB: ROA);')
      .newErrors(
        `
          test.js:37
           37: (woB: ROA);
                ^^^ object type. Contravariant property \`p\` incompatible with covariant use in
           37: (woB: ROA);
                     ^^^ object type
        `,
      ),
  ]),

  test('X ~> -A', [
    // literal A
    addCode('({p: new A}: WOA);').noNewErrors(),

    // A
    addCode('(rwA: WOA);').noNewErrors(),

    // +A
    addCode('(roA: WOA);')
      .newErrors(
        `
          test.js:27
           27: (roA: WOA);
                ^^^ object type. Covariant property \`p\` incompatible with contravariant use in
           27: (roA: WOA);
                     ^^^ object type
        `,
      ),

    // -A
    addCode('(woA: WOA);').noNewErrors(),

    // literal B
    addCode('({p: new B}: WOA);').noNewErrors(),

    // B
    addCode('(rwB: WOA);')
      .newErrors(
        `
          test.js:9
            9:       type WOA = {-p: A}
                                     ^ A. This type is incompatible with
           10:       type RWB = {p: B}
                                    ^ B
        `,
      ),

    // +B
    addCode('(roB: WOA);')
      .newErrors(
        `
          test.js:35
           35: (roB: WOA);
                ^^^ object type. Covariant property \`p\` incompatible with contravariant use in
           35: (roB: WOA);
                     ^^^ object type
        `,
      ),

    // -B
    addCode('(woB: WOA);')
      .newErrors(
        `
          test.js:9
            9:       type WOA = {-p: A}
                                     ^ A. This type is incompatible with
           12:       type WOB = {-p: B}
                                     ^ B
        `,
      ),
  ]),

  test('X ~> B', [
    // literal A
    addCode('({p: new A}: RWB);')
      .newErrors(
        `
          test.js:23
           23: ({p: new A}: RWB);
                    ^^^^^ A. This type is incompatible with
           10:     type RWB = {p: B}
                                  ^ B
        `,
      ),

    // A
    addCode('(rwA: RWB);')
      .newErrors(
        `
          test.js:7
            7:       type RWA = {p: A}
                                    ^ A. This type is incompatible with
           10:       type RWB = {p: B}
                                    ^ B
        `,
      ),

    // +A
    addCode('(roA: RWB);')
      .newErrors(
        `
          test.js:8
            8:     type ROA = {+p: A}
                                   ^ A. This type is incompatible with
           10:     type RWB = {p: B}
                                  ^ B

          test.js:27
           27: (roA: RWB);
                ^^^ object type. Covariant property \`p\` incompatible with invariant use in
           27: (roA: RWB);
                     ^^^ object type
        `,
      ),

    // -A
    addCode('(woA: RWB);')
      .newErrors(
        `
          test.js:29
           29: (woA: RWB);
                ^^^ object type. Contravariant property \`p\` incompatible with invariant use in
           29: (woA: RWB);
                     ^^^ object type
        `,
      ),
  ]),

  test('X ~> +B', [
    // literal A
    addCode('({p: new A}: ROB);')
      .newErrors(
        `
          test.js:23
           23: ({p: new A}: ROB);
                    ^^^^^ A. This type is incompatible with
           11:     type ROB = {+p: B}
                                   ^ B
        `,
      ),

    // A
    addCode('(rwA: ROB);')
      .newErrors(
        `
          test.js:7
            7:       type RWA = {p: A}
                                    ^ A. This type is incompatible with
           11:       type ROB = {+p: B}
                                     ^ B
        `,
      ),

    // +A
    addCode('(roA: ROB);')
      .newErrors(
        `
          test.js:8
            8:       type ROA = {+p: A}
                                     ^ A. This type is incompatible with
           11:       type ROB = {+p: B}
                                     ^ B
        `,
      ),

    // -A
    addCode('(woA: ROB);')
      .newErrors(
        `
          test.js:29
           29: (woA: ROB);
                ^^^ object type. Contravariant property \`p\` incompatible with covariant use in
           29: (woA: ROB);
                     ^^^ object type
        `,
      ),
  ]),

  test('X ~> -B', [
    // literal A
    addCode('({p: new A}: WOB);').noNewErrors(),

    // A
    addCode('(rwA: WOB);').noNewErrors(),

    // +A
    addCode('(roA: WOB);')
      .newErrors(
        `
          test.js:27
           27: (roA: WOB);
                ^^^ object type. Covariant property \`p\` incompatible with contravariant use in
           27: (roA: WOB);
                     ^^^ object type
        `,
      ),

    // -A
    addCode('(woA: WOB);').noNewErrors(),
  ]),

  test('unification', [
    // Note: these tests don't reuse the type aliases from the prelude because
    // doing so results in "naive" unification instead of rec_unify.

    addCode('(([rwA]: Array<{p:A}>): Array<{p:A}>);')
      .noNewErrors(),

    addCode('(([roA]: Array<{+p:A}>): Array<{p:A}>);')
      .newErrors(
        `
          test.js:25
           25: (([roA]: Array<{+p:A}>): Array<{p:A}>);
                              ^^^^^^ object type. Covariant property \`p\` incompatible with invariant use in
           25: (([roA]: Array<{+p:A}>): Array<{p:A}>);
                                              ^^^^^ object type
        `,
      ),

    addCode('(([woA]: Array<{-p:A}>): Array<{p:A}>);')
      .newErrors(
        `
          test.js:27
           27: (([woA]: Array<{-p:A}>): Array<{p:A}>);
                              ^^^^^^ object type. Contravariant property \`p\` incompatible with invariant use in
           27: (([woA]: Array<{-p:A}>): Array<{p:A}>);
                                              ^^^^^ object type
        `,
      ),

    addCode('(([rwA]: Array<{p:A}>): Array<{+p:A}>);')
      .newErrors(
        `
          test.js:29
           29: (([rwA]: Array<{p:A}>): Array<{+p:A}>);
                              ^^^^^ object type. Invariant property \`p\` incompatible with covariant use in
           29: (([rwA]: Array<{p:A}>): Array<{+p:A}>);
                                             ^^^^^^ object type
        `,
      ),

    addCode('(([roA]: Array<{+p:A}>): Array<{+p:A}>);')
      .noNewErrors(),

    addCode('(([woA]: Array<{-p:A}>): Array<{+p:A}>);')
      .newErrors(
        `
          test.js:33
           33: (([woA]: Array<{-p:A}>): Array<{+p:A}>);
                              ^^^^^^ object type. Contravariant property \`p\` incompatible with covariant use in
           33: (([woA]: Array<{-p:A}>): Array<{+p:A}>);
                                              ^^^^^^ object type
        `,
      ),

    addCode('(([rwA]: Array<{p:A}>): Array<{-p:A}>);')
      .newErrors(
        `
          test.js:35
           35: (([rwA]: Array<{p:A}>): Array<{-p:A}>);
                              ^^^^^ object type. Invariant property \`p\` incompatible with contravariant use in
           35: (([rwA]: Array<{p:A}>): Array<{-p:A}>);
                                             ^^^^^^ object type
        `,
      ),

    addCode('(([roA]: Array<{+p:A}>): Array<{-p:A}>);')
      .newErrors(
        `
          test.js:37
           37: (([roA]: Array<{+p:A}>): Array<{-p:A}>);
                              ^^^^^^ object type. Covariant property \`p\` incompatible with contravariant use in
           37: (([roA]: Array<{+p:A}>): Array<{-p:A}>);
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
         test.js:27
          27: (woA: \$Shape<RWA>);
               ^^^ object type. Contravariant property \`p\` incompatible with covariant use in
          27: (woA: \$Shape<RWA>);
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
         test.js:33
          33: (woB: \$Shape<RWA>);
               ^^^ object type. Contravariant property \`p\` incompatible with covariant use in
          33: (woB: \$Shape<RWA>);
                           ^^^ RWA
       `,
     ),

    // A ~> B
    addCode('(rwA: $Shape<RWB>);').
      newErrors(
       `
         test.js:7
           7:     type RWA = {p: A}
                                 ^ A. This type is incompatible with
          10:     type RWB = {p: B}
                                 ^ B
       `,
     ),

    // +A ~> B
    addCode('(roA: $Shape<RWB>);').
      newErrors(
       `
         test.js:8
           8:     type ROA = {+p: A}
                                  ^ A. This type is incompatible with
          10:     type RWB = {p: B}
                                 ^ B
       `,
     ),

    // -A ~> B
    addCode('(woA: $Shape<RWB>);').
      newErrors(
       `
         test.js:39
          39: (woA: \$Shape<RWB>);
               ^^^ object type. Contravariant property \`p\` incompatible with covariant use in
          39: (woA: \$Shape<RWB>);
                           ^^^ RWB
       `,
     ),
  ]),

]).beforeEach(({addCode}) => [
  addCode(`
    class A {}
    class B extends A {};

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
  `),
]);
