/*
 * @flow
 * @lint-ignore-every LINEWRAP1
 */

import {suite, test} from '../../packages/flow-dev-tools/src/test/Tester';

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
                ^^^ ROA. This type is incompatible with
           50: (roA: RWA);
                     ^^^ RWA
            Property \`p\` is incompatible:
               50: (roA: RWA);
                    ^^^ ROA. Covariant property \`p\` incompatible with invariant use in
               50: (roA: RWA);
                         ^^^ RWA
        `,
      ),
    addCode('(droA: dRWA);')
      .newErrors(
        `
          test.js:52
           52: (droA: dRWA);
                ^^^^ dROA. This type is incompatible with
           52: (droA: dRWA);
                      ^^^^ dRWA
            Indexable signature is incompatible:
               52: (droA: dRWA);
                    ^^^^ dROA. Covariant computed property incompatible with invariant use in
               52: (droA: dRWA);
                          ^^^^ dRWA
        `,
      ),

    // -A
    addCode('(woA: RWA);')
      .newErrors(
        `
          test.js:54
           54: (woA: RWA);
                ^^^ WOA. This type is incompatible with
           54: (woA: RWA);
                     ^^^ RWA
            Property \`p\` is incompatible:
               54: (woA: RWA);
                    ^^^ WOA. Contravariant property \`p\` incompatible with invariant use in
               54: (woA: RWA);
                         ^^^ RWA
        `,
      ),
    addCode('(dwoA: dRWA);')
      .newErrors(
        `
          test.js:56
           56: (dwoA: dRWA);
                ^^^^ dWOA. This type is incompatible with
           56: (dwoA: dRWA);
                      ^^^^ dRWA
            Indexable signature is incompatible:
               56: (dwoA: dRWA);
                    ^^^^ dWOA. Contravariant computed property incompatible with invariant use in
               56: (dwoA: dRWA);
                          ^^^^ dRWA
        `,
      ),


    // literal B
    addCode('({p: new B}: RWA);').noNewErrors(),
    addCode('({p: new B}: dRWA);').noNewErrors(),

    // B
    addCode('(rwB: RWA);')
      .newErrors(
        `
          test.js:62
           62: (rwB: RWA);
                ^^^ Cannot cast \`rwB\` to \`RWA\` because in property \`p\`, \`B\` [1] is incompatible with \`A\` [2].
            References:
             12:     type RWB = {p: B}
                                    ^ [1]: \`B\`
              9:     type RWA = {p: A}
                                    ^ [2]: \`A\`
        `,
      ),
    addCode('(drwB: dRWA);')
      .newErrors(
        `
          test.js:64
           64: (drwB: dRWA);
                ^^^^ Cannot cast \`drwB\` to \`dRWA\` because in the indexer property, \`B\` [1] is incompatible with \`A\` [2].
            References:
             29:     type dRWB = {[string]: B};
                                            ^ [1]: \`B\`
             26:     type dRWA = {[string]: A};
                                            ^ [2]: \`A\`
        `,
      ),

    // +B
    addCode('(roB: RWA);')
      .newErrors(
        `
          test.js:66
           66: (roB: RWA);
                ^^^ ROB. This type is incompatible with
           66: (roB: RWA);
                     ^^^ RWA
            Property \`p\` is incompatible:
               66: (roB: RWA);
                    ^^^ ROB. Covariant property \`p\` incompatible with invariant use in
               66: (roB: RWA);
                         ^^^ RWA
        `,
      ),
    addCode('(droB: dRWA);')
      .newErrors(
        `
          test.js:68
           68: (droB: dRWA);
                ^^^^ dROB. This type is incompatible with
           68: (droB: dRWA);
                      ^^^^ dRWA
            Indexable signature is incompatible:
               68: (droB: dRWA);
                    ^^^^ dROB. Covariant computed property incompatible with invariant use in
               68: (droB: dRWA);
                          ^^^^ dRWA
        `,
      ),

    // -B
    addCode('(woB: RWA);')
      .newErrors(
        `
          test.js:70
           70: (woB: RWA);
                ^^^ WOB. This type is incompatible with
           70: (woB: RWA);
                     ^^^ RWA
            Property \`p\` is incompatible:
               70: (woB: RWA);
                    ^^^ WOB. Contravariant property \`p\` incompatible with invariant use in
               70: (woB: RWA);
                         ^^^ RWA

          test.js:70
           70: (woB: RWA);
                ^^^ Cannot cast \`woB\` to \`RWA\` because in property \`p\`, \`A\` [1] is incompatible with \`B\` [2].
            References:
              9:     type RWA = {p: A}
                                    ^ [1]: \`A\`
             14:     type WOB = {-p: B}
                                     ^ [2]: \`B\`
        `,
      ),
    addCode('(dwoB: dRWA);')
      .newErrors(
        `
          test.js:72
           72: (dwoB: dRWA);
                ^^^^ dWOB. This type is incompatible with
           72: (dwoB: dRWA);
                      ^^^^ dRWA
            Indexable signature is incompatible:
               72: (dwoB: dRWA);
                    ^^^^ dWOB. Contravariant computed property incompatible with invariant use in
               72: (dwoB: dRWA);
                          ^^^^ dRWA

          test.js:72
           72: (dwoB: dRWA);
                ^^^^ Cannot cast \`dwoB\` to \`dRWA\` because in the indexer property, \`A\` [1] is incompatible with \`B\` [2].
            References:
             26:     type dRWA = {[string]: A};
                                            ^ [1]: \`A\`
             31:     type dWOB = {-[string]: B};
                                             ^ [2]: \`B\`
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
                ^^^ WOA. This type is incompatible with
           54: (woA: ROA);
                     ^^^ ROA
            Property \`p\` is incompatible:
               54: (woA: ROA);
                    ^^^ WOA. Contravariant property \`p\` incompatible with covariant use in
               54: (woA: ROA);
                         ^^^ ROA
        `,
      ),
    addCode('(dwoA: dROA);')
      .newErrors(
        `
          test.js:56
           56: (dwoA: dROA);
                ^^^^ dWOA. This type is incompatible with
           56: (dwoA: dROA);
                      ^^^^ dROA
            Indexable signature is incompatible:
               56: (dwoA: dROA);
                    ^^^^ dWOA. Contravariant computed property incompatible with covariant use in
               56: (dwoA: dROA);
                          ^^^^ dROA
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
                ^^^ WOB. This type is incompatible with
           70: (woB: ROA);
                     ^^^ ROA
            Property \`p\` is incompatible:
               70: (woB: ROA);
                    ^^^ WOB. Contravariant property \`p\` incompatible with covariant use in
               70: (woB: ROA);
                         ^^^ ROA
        `,
      ),
    addCode('(dwoB: dROA);')
      .newErrors(
        `
          test.js:72
           72: (dwoB: dROA);
                ^^^^ dWOB. This type is incompatible with
           72: (dwoB: dROA);
                      ^^^^ dROA
            Indexable signature is incompatible:
               72: (dwoB: dROA);
                    ^^^^ dWOB. Contravariant computed property incompatible with covariant use in
               72: (dwoB: dROA);
                          ^^^^ dROA
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
                ^^^ ROA. This type is incompatible with
           50: (roA: WOA);
                     ^^^ WOA
            Property \`p\` is incompatible:
               50: (roA: WOA);
                    ^^^ ROA. Covariant property \`p\` incompatible with contravariant use in
               50: (roA: WOA);
                         ^^^ WOA
        `,
      ),
    addCode('(droA: dWOA);')
      .newErrors(
        `
          test.js:52
           52: (droA: dWOA);
                ^^^^ dROA. This type is incompatible with
           52: (droA: dWOA);
                      ^^^^ dWOA
            Indexable signature is incompatible:
               52: (droA: dWOA);
                    ^^^^ dROA. Covariant computed property incompatible with contravariant use in
               52: (droA: dWOA);
                          ^^^^ dWOA
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
          test.js:62
           62: (rwB: WOA);
                ^^^ Cannot cast \`rwB\` to \`WOA\` because in property \`p\`, \`A\` [1] is incompatible with \`B\` [2].
            References:
             11:     type WOA = {-p: A}
                                     ^ [1]: \`A\`
             12:     type RWB = {p: B}
                                    ^ [2]: \`B\`
        `,
      ),
    addCode('(drwB: dWOA);')
      .newErrors(
        `
          test.js:64
           64: (drwB: dWOA);
                ^^^^ Cannot cast \`drwB\` to \`dWOA\` because in the indexer property, \`A\` [1] is incompatible with \`B\` [2].
            References:
             28:     type dWOA = {-[string]: A};
                                             ^ [1]: \`A\`
             29:     type dRWB = {[string]: B};
                                            ^ [2]: \`B\`
        `,
      ),

    // +B
    addCode('(roB: WOA);')
      .newErrors(
        `
          test.js:66
           66: (roB: WOA);
                ^^^ ROB. This type is incompatible with
           66: (roB: WOA);
                     ^^^ WOA
            Property \`p\` is incompatible:
               66: (roB: WOA);
                    ^^^ ROB. Covariant property \`p\` incompatible with contravariant use in
               66: (roB: WOA);
                         ^^^ WOA
        `,
      ),
    addCode('(droB: dWOA);')
      .newErrors(
        `
          test.js:68
           68: (droB: dWOA);
                ^^^^ dROB. This type is incompatible with
           68: (droB: dWOA);
                      ^^^^ dWOA
            Indexable signature is incompatible:
               68: (droB: dWOA);
                    ^^^^ dROB. Covariant computed property incompatible with contravariant use in
               68: (droB: dWOA);
                          ^^^^ dWOA
        `,
      ),

    // -B
    addCode('(woB: WOA);')
      .newErrors(
        `
          test.js:70
           70: (woB: WOA);
                ^^^ Cannot cast \`woB\` to \`WOA\` because in property \`p\`, \`A\` [1] is incompatible with \`B\` [2].
            References:
             11:     type WOA = {-p: A}
                                     ^ [1]: \`A\`
             14:     type WOB = {-p: B}
                                     ^ [2]: \`B\`
        `,
      ),
    addCode('(dwoB: dWOA);')
      .newErrors(
        `
          test.js:72
           72: (dwoB: dWOA);
                ^^^^ Cannot cast \`dwoB\` to \`dWOA\` because in the indexer property, \`A\` [1] is incompatible with \`B\` [2].
            References:
             28:     type dWOA = {-[string]: A};
                                             ^ [1]: \`A\`
             31:     type dWOB = {-[string]: B};
                                             ^ [2]: \`B\`
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
                    ^^^^^ Cannot cast object literal to \`RWB\` because in property \`p\`, \`A\` [1] is incompatible with \`B\` [2].
            References:
             42: ({p: new A}: RWB);
                      ^^^^^ [1]: \`A\`
             12:     type RWB = {p: B}
                                    ^ [2]: \`B\`
        `,
      ),
    addCode('({p: new A}: dRWB);')
      .newErrors(
        `
          test.js:44
           44: ({p: new A}: dRWB);
                    ^^^^^ Cannot cast object literal to \`dRWB\` because in property \`p\`, \`A\` [1] is incompatible with \`B\` [2].
            References:
             44: ({p: new A}: dRWB);
                      ^^^^^ [1]: \`A\`
             29:     type dRWB = {[string]: B};
                                            ^ [2]: \`B\`
        `,
      ),

    // A
    addCode('(rwA: RWB);')
      .newErrors(
        `
          test.js:46
           46: (rwA: RWB);
                ^^^ Cannot cast \`rwA\` to \`RWB\` because in property \`p\`, \`A\` [1] is incompatible with \`B\` [2].
            References:
              9:     type RWA = {p: A}
                                    ^ [1]: \`A\`
             12:     type RWB = {p: B}
                                    ^ [2]: \`B\`
        `,
      ),
    addCode('(drwA: dRWB);')
      .newErrors(
        `
          test.js:48
           48: (drwA: dRWB);
                ^^^^ Cannot cast \`drwA\` to \`dRWB\` because in the indexer property, \`A\` [1] is incompatible with \`B\` [2].
            References:
             26:     type dRWA = {[string]: A};
                                            ^ [1]: \`A\`
             29:     type dRWB = {[string]: B};
                                            ^ [2]: \`B\`
        `,
      ),

    // +A
    addCode('(roA: RWB);')
      .newErrors(
        `
          test.js:50
           50: (roA: RWB);
                ^^^ ROA. This type is incompatible with
           50: (roA: RWB);
                     ^^^ RWB
            Property \`p\` is incompatible:
               50: (roA: RWB);
                    ^^^ ROA. Covariant property \`p\` incompatible with invariant use in
               50: (roA: RWB);
                         ^^^ RWB

          test.js:50
           50: (roA: RWB);
                ^^^ Cannot cast \`roA\` to \`RWB\` because in property \`p\`, \`A\` [1] is incompatible with \`B\` [2].
            References:
             10:     type ROA = {+p: A}
                                     ^ [1]: \`A\`
             12:     type RWB = {p: B}
                                    ^ [2]: \`B\`
        `,
      ),
    addCode('(droA: dRWB);')
      .newErrors(
        `
          test.js:52
           52: (droA: dRWB);
                ^^^^ dROA. This type is incompatible with
           52: (droA: dRWB);
                      ^^^^ dRWB
            Indexable signature is incompatible:
               52: (droA: dRWB);
                    ^^^^ dROA. Covariant computed property incompatible with invariant use in
               52: (droA: dRWB);
                          ^^^^ dRWB

          test.js:52
           52: (droA: dRWB);
                ^^^^ Cannot cast \`droA\` to \`dRWB\` because in the indexer property, \`A\` [1] is incompatible with \`B\` [2].
            References:
             27:     type dROA = {+[string]: A};
                                             ^ [1]: \`A\`
             29:     type dRWB = {[string]: B};
                                            ^ [2]: \`B\`
        `,
      ),

    // -A
    addCode('(woA: RWB);')
      .newErrors(
        `
          test.js:54
           54: (woA: RWB);
                ^^^ WOA. This type is incompatible with
           54: (woA: RWB);
                     ^^^ RWB
            Property \`p\` is incompatible:
               54: (woA: RWB);
                    ^^^ WOA. Contravariant property \`p\` incompatible with invariant use in
               54: (woA: RWB);
                         ^^^ RWB
        `,
      ),
    addCode('(dwoA: dRWB);')
      .newErrors(
        `
          test.js:56
           56: (dwoA: dRWB);
                ^^^^ dWOA. This type is incompatible with
           56: (dwoA: dRWB);
                      ^^^^ dRWB
            Indexable signature is incompatible:
               56: (dwoA: dRWB);
                    ^^^^ dWOA. Contravariant computed property incompatible with invariant use in
               56: (dwoA: dRWB);
                          ^^^^ dRWB
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
                    ^^^^^ Cannot cast object literal to \`ROB\` because in property \`p\`, \`A\` [1] is incompatible with \`B\` [2].
            References:
             42: ({p: new A}: ROB);
                      ^^^^^ [1]: \`A\`
             13:     type ROB = {+p: B}
                                     ^ [2]: \`B\`
        `,
      ),
    addCode('({p: new A}: dROB);')
      .newErrors(
        `
          test.js:44
           44: ({p: new A}: dROB);
                    ^^^^^ Cannot cast object literal to \`dROB\` because in property \`p\`, \`A\` [1] is incompatible with \`B\` [2].
            References:
             44: ({p: new A}: dROB);
                      ^^^^^ [1]: \`A\`
             30:     type dROB = {+[string]: B};
                                             ^ [2]: \`B\`
        `,
      ),

    // A
    addCode('(rwA: ROB);')
      .newErrors(
        `
          test.js:46
           46: (rwA: ROB);
                ^^^ Cannot cast \`rwA\` to \`ROB\` because in property \`p\`, \`A\` [1] is incompatible with \`B\` [2].
            References:
              9:     type RWA = {p: A}
                                    ^ [1]: \`A\`
             13:     type ROB = {+p: B}
                                     ^ [2]: \`B\`
        `,
      ),
    addCode('(drwA: dROB);')
      .newErrors(
        `
          test.js:48
           48: (drwA: dROB);
                ^^^^ Cannot cast \`drwA\` to \`dROB\` because in the indexer property, \`A\` [1] is incompatible with \`B\` [2].
            References:
             26:     type dRWA = {[string]: A};
                                            ^ [1]: \`A\`
             30:     type dROB = {+[string]: B};
                                             ^ [2]: \`B\`
        `,
      ),

    // +A
    addCode('(roA: ROB);')
      .newErrors(
        `
          test.js:50
           50: (roA: ROB);
                ^^^ Cannot cast \`roA\` to \`ROB\` because in property \`p\`, \`A\` [1] is incompatible with \`B\` [2].
            References:
             10:     type ROA = {+p: A}
                                     ^ [1]: \`A\`
             13:     type ROB = {+p: B}
                                     ^ [2]: \`B\`
        `,
      ),
    addCode('(droA: dROB);')
      .newErrors(
        `
          test.js:52
           52: (droA: dROB);
                ^^^^ Cannot cast \`droA\` to \`dROB\` because in the indexer property, \`A\` [1] is incompatible with \`B\` [2].
            References:
             27:     type dROA = {+[string]: A};
                                             ^ [1]: \`A\`
             30:     type dROB = {+[string]: B};
                                             ^ [2]: \`B\`
        `,
      ),

    // -A
    addCode('(woA: ROB);')
      .newErrors(
        `
          test.js:54
           54: (woA: ROB);
                ^^^ WOA. This type is incompatible with
           54: (woA: ROB);
                     ^^^ ROB
            Property \`p\` is incompatible:
               54: (woA: ROB);
                    ^^^ WOA. Contravariant property \`p\` incompatible with covariant use in
               54: (woA: ROB);
                         ^^^ ROB
        `,
      ),
    addCode('(dwoA: dROB);')
      .newErrors(
        `
          test.js:56
           56: (dwoA: dROB);
                ^^^^ dWOA. This type is incompatible with
           56: (dwoA: dROB);
                      ^^^^ dROB
            Indexable signature is incompatible:
               56: (dwoA: dROB);
                    ^^^^ dWOA. Contravariant computed property incompatible with covariant use in
               56: (dwoA: dROB);
                          ^^^^ dROB
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
                ^^^ ROA. This type is incompatible with
           50: (roA: WOB);
                     ^^^ WOB
            Property \`p\` is incompatible:
               50: (roA: WOB);
                    ^^^ ROA. Covariant property \`p\` incompatible with contravariant use in
               50: (roA: WOB);
                         ^^^ WOB
        `,
      ),
    addCode('(droA: dWOB);')
      .newErrors(
        `
          test.js:52
           52: (droA: dWOB);
                ^^^^ dROA. This type is incompatible with
           52: (droA: dWOB);
                      ^^^^ dWOB
            Indexable signature is incompatible:
               52: (droA: dWOB);
                    ^^^^ dROA. Covariant computed property incompatible with contravariant use in
               52: (droA: dWOB);
                          ^^^^ dWOB
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
                        ^^^^^^^^^^^^^ array type. Has some incompatible type argument with
           44: (([roA]: Array<{+p:A}>): Array<{p:A}>);
                                        ^^^^^^^^^^^^ array type
            Type argument \`T\` is incompatible:
               44: (([roA]: Array<{+p:A}>): Array<{p:A}>);
                                  ^^^^^^ object type. This type is incompatible with
               44: (([roA]: Array<{+p:A}>): Array<{p:A}>);
                                                  ^^^^^ object type
                Property \`p\` is incompatible:
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
                        ^^^^^^^^^^^^^ array type. Has some incompatible type argument with
           46: (([woA]: Array<{-p:A}>): Array<{p:A}>);
                                        ^^^^^^^^^^^^ array type
            Type argument \`T\` is incompatible:
               46: (([woA]: Array<{-p:A}>): Array<{p:A}>);
                                  ^^^^^^ object type. This type is incompatible with
               46: (([woA]: Array<{-p:A}>): Array<{p:A}>);
                                                  ^^^^^ object type
                Property \`p\` is incompatible:
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
                        ^^^^^^^^^^^^ array type. Has some incompatible type argument with
           48: (([rwA]: Array<{p:A}>): Array<{+p:A}>);
                                       ^^^^^^^^^^^^^ array type
            Type argument \`T\` is incompatible:
               48: (([rwA]: Array<{p:A}>): Array<{+p:A}>);
                                  ^^^^^ object type. This type is incompatible with
               48: (([rwA]: Array<{p:A}>): Array<{+p:A}>);
                                                 ^^^^^^ object type
                Property \`p\` is incompatible:
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
                        ^^^^^^^^^^^^^ array type. Has some incompatible type argument with
           52: (([woA]: Array<{-p:A}>): Array<{+p:A}>);
                                        ^^^^^^^^^^^^^ array type
            Type argument \`T\` is incompatible:
               52: (([woA]: Array<{-p:A}>): Array<{+p:A}>);
                                  ^^^^^^ object type. This type is incompatible with
               52: (([woA]: Array<{-p:A}>): Array<{+p:A}>);
                                                  ^^^^^^ object type
                Property \`p\` is incompatible:
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
                        ^^^^^^^^^^^^ array type. Has some incompatible type argument with
           54: (([rwA]: Array<{p:A}>): Array<{-p:A}>);
                                       ^^^^^^^^^^^^^ array type
            Type argument \`T\` is incompatible:
               54: (([rwA]: Array<{p:A}>): Array<{-p:A}>);
                                  ^^^^^ object type. This type is incompatible with
               54: (([rwA]: Array<{p:A}>): Array<{-p:A}>);
                                                 ^^^^^^ object type
                Property \`p\` is incompatible:
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
                        ^^^^^^^^^^^^^ array type. Has some incompatible type argument with
           56: (([roA]: Array<{+p:A}>): Array<{-p:A}>);
                                        ^^^^^^^^^^^^^ array type
            Type argument \`T\` is incompatible:
               56: (([roA]: Array<{+p:A}>): Array<{-p:A}>);
                                  ^^^^^^ object type. This type is incompatible with
               56: (([roA]: Array<{+p:A}>): Array<{-p:A}>);
                                                  ^^^^^^ object type
                Property \`p\` is incompatible:
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
          46: (woA: $Shape<RWA>);
               ^^^ WOA. Contravariant property \`p\` incompatible with covariant use in
          46: (woA: $Shape<RWA>);
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
          52: (woB: $Shape<RWA>);
               ^^^ WOB. Contravariant property \`p\` incompatible with covariant use in
          52: (woB: $Shape<RWA>);
                           ^^^ RWA
       `,
     ),

    // A ~> B
    addCode('(rwA: $Shape<RWB>);').
      newErrors(
       `
         test.js:54
          54: (rwA: $Shape<RWB>);
               ^^^ Cannot cast \`rwA\` to \`RWB\` because in property \`p\`, \`A\` [1] is incompatible with \`B\` [2].
           References:
             9:     type RWA = {p: A}
                                   ^ [1]: \`A\`
            12:     type RWB = {p: B}
                                   ^ [2]: \`B\`
       `,
     ),

    // +A ~> B
    addCode('(roA: $Shape<RWB>);').
      newErrors(
       `
         test.js:56
          56: (roA: $Shape<RWB>);
               ^^^ Cannot cast \`roA\` to \`RWB\` because in property \`p\`, \`A\` [1] is incompatible with \`B\` [2].
           References:
            10:     type ROA = {+p: A}
                                    ^ [1]: \`A\`
            12:     type RWB = {p: B}
                                   ^ [2]: \`B\`
       `,
     ),

    // -A ~> B
    addCode('(woA: $Shape<RWB>);').
      newErrors(
       `
         test.js:58
          58: (woA: $Shape<RWB>);
               ^^^ WOA. Contravariant property \`p\` incompatible with covariant use in
          58: (woA: $Shape<RWB>);
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
