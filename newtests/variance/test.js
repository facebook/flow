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
                ^^^ Cannot cast \`roA\` to \`RWA\` because property \`p\` is read-only in \`ROA\` [1] but writable in \`RWA\` [2].
            References:
             17:     declare var roA: ROA;
                                      ^^^ [1]: \`ROA\`
             50: (roA: RWA);
                       ^^^ [2]: \`RWA\`
        `,
      ),
    addCode('(droA: dRWA);')
      .newErrors(
        `
          test.js:52
           52: (droA: dRWA);
                ^^^^ Cannot cast \`droA\` to \`dRWA\` because an indexer property is read-only in \`dROA\` [1] but writable in \`dRWA\` [2].
            References:
             34:     declare var droA: dROA;
                                       ^^^^ [1]: \`dROA\`
             52: (droA: dRWA);
                        ^^^^ [2]: \`dRWA\`
        `,
      ),

    // -A
    addCode('(woA: RWA);')
      .newErrors(
        `
          test.js:54
           54: (woA: RWA);
                ^^^ Cannot cast \`woA\` to \`RWA\` because property \`p\` is write-only in \`WOA\` [1] but readable in \`RWA\` [2].
            References:
             18:     declare var woA: WOA;
                                      ^^^ [1]: \`WOA\`
             54: (woA: RWA);
                       ^^^ [2]: \`RWA\`
        `,
      ),
    addCode('(dwoA: dRWA);')
      .newErrors(
        `
          test.js:56
           56: (dwoA: dRWA);
                ^^^^ Cannot cast \`dwoA\` to \`dRWA\` because an indexer property is write-only in \`dWOA\` [1] but readable in \`dRWA\` [2].
            References:
             35:     declare var dwoA: dWOA;
                                       ^^^^ [1]: \`dWOA\`
             56: (dwoA: dRWA);
                        ^^^^ [2]: \`dRWA\`
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
                ^^^ Cannot cast \`roB\` to \`RWA\` because property \`p\` is read-only in \`ROB\` [1] but writable in \`RWA\` [2].
            References:
             21:     declare var roB: ROB;
                                      ^^^ [1]: \`ROB\`
             66: (roB: RWA);
                       ^^^ [2]: \`RWA\`
        `,
      ),
    addCode('(droB: dRWA);')
      .newErrors(
        `
          test.js:68
           68: (droB: dRWA);
                ^^^^ Cannot cast \`droB\` to \`dRWA\` because an indexer property is read-only in \`dROB\` [1] but writable in \`dRWA\` [2].
            References:
             38:     declare var droB: dROB;
                                       ^^^^ [1]: \`dROB\`
             68: (droB: dRWA);
                        ^^^^ [2]: \`dRWA\`
        `,
      ),

    // -B
    addCode('(woB: RWA);')
      .newErrors(
        `
          test.js:70
           70: (woB: RWA);
                ^^^ Cannot cast \`woB\` to \`RWA\` because in property \`p\`, \`A\` [1] is incompatible with \`B\` [2].
            References:
              9:     type RWA = {p: A}
                                    ^ [1]: \`A\`
             14:     type WOB = {-p: B}
                                     ^ [2]: \`B\`

          test.js:70
           70: (woB: RWA);
                ^^^ Cannot cast \`woB\` to \`RWA\` because property \`p\` is write-only in \`WOB\` [1] but readable in \`RWA\` [2].
            References:
             22:     declare var woB: WOB;
                                      ^^^ [1]: \`WOB\`
             70: (woB: RWA);
                       ^^^ [2]: \`RWA\`
        `,
      ),
    addCode('(dwoB: dRWA);')
      .newErrors(
        `
          test.js:72
           72: (dwoB: dRWA);
                ^^^^ Cannot cast \`dwoB\` to \`dRWA\` because an indexer property is write-only in \`dWOB\` [1] but readable in \`dRWA\` [2].
            References:
             39:     declare var dwoB: dWOB;
                                       ^^^^ [1]: \`dWOB\`
             72: (dwoB: dRWA);
                        ^^^^ [2]: \`dRWA\`

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
                ^^^ Cannot cast \`woA\` to \`ROA\` because property \`p\` is write-only in \`WOA\` [1] but read-only in \`ROA\` [2].
            References:
             18:     declare var woA: WOA;
                                      ^^^ [1]: \`WOA\`
             54: (woA: ROA);
                       ^^^ [2]: \`ROA\`
        `,
      ),
    addCode('(dwoA: dROA);')
      .newErrors(
        `
          test.js:56
           56: (dwoA: dROA);
                ^^^^ Cannot cast \`dwoA\` to \`dROA\` because an indexer property is write-only in \`dWOA\` [1] but read-only in \`dROA\` [2].
            References:
             35:     declare var dwoA: dWOA;
                                       ^^^^ [1]: \`dWOA\`
             56: (dwoA: dROA);
                        ^^^^ [2]: \`dROA\`
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
                ^^^ Cannot cast \`woB\` to \`ROA\` because property \`p\` is write-only in \`WOB\` [1] but read-only in \`ROA\` [2].
            References:
             22:     declare var woB: WOB;
                                      ^^^ [1]: \`WOB\`
             70: (woB: ROA);
                       ^^^ [2]: \`ROA\`
        `,
      ),
    addCode('(dwoB: dROA);')
      .newErrors(
        `
          test.js:72
           72: (dwoB: dROA);
                ^^^^ Cannot cast \`dwoB\` to \`dROA\` because an indexer property is write-only in \`dWOB\` [1] but read-only in \`dROA\` [2].
            References:
             39:     declare var dwoB: dWOB;
                                       ^^^^ [1]: \`dWOB\`
             72: (dwoB: dROA);
                        ^^^^ [2]: \`dROA\`
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
                ^^^ Cannot cast \`roA\` to \`WOA\` because property \`p\` is read-only in \`ROA\` [1] but write-only in \`WOA\` [2].
            References:
             17:     declare var roA: ROA;
                                      ^^^ [1]: \`ROA\`
             50: (roA: WOA);
                       ^^^ [2]: \`WOA\`
        `,
      ),
    addCode('(droA: dWOA);')
      .newErrors(
        `
          test.js:52
           52: (droA: dWOA);
                ^^^^ Cannot cast \`droA\` to \`dWOA\` because an indexer property is read-only in \`dROA\` [1] but write-only in \`dWOA\` [2].
            References:
             34:     declare var droA: dROA;
                                       ^^^^ [1]: \`dROA\`
             52: (droA: dWOA);
                        ^^^^ [2]: \`dWOA\`
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
                ^^^ Cannot cast \`roB\` to \`WOA\` because property \`p\` is read-only in \`ROB\` [1] but write-only in \`WOA\` [2].
            References:
             21:     declare var roB: ROB;
                                      ^^^ [1]: \`ROB\`
             66: (roB: WOA);
                       ^^^ [2]: \`WOA\`
        `,
      ),
    addCode('(droB: dWOA);')
      .newErrors(
        `
          test.js:68
           68: (droB: dWOA);
                ^^^^ Cannot cast \`droB\` to \`dWOA\` because an indexer property is read-only in \`dROB\` [1] but write-only in \`dWOA\` [2].
            References:
             38:     declare var droB: dROB;
                                       ^^^^ [1]: \`dROB\`
             68: (droB: dWOA);
                        ^^^^ [2]: \`dWOA\`
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
                ^^^ Cannot cast \`roA\` to \`RWB\` because in property \`p\`, \`A\` [1] is incompatible with \`B\` [2].
            References:
             10:     type ROA = {+p: A}
                                     ^ [1]: \`A\`
             12:     type RWB = {p: B}
                                    ^ [2]: \`B\`

          test.js:50
           50: (roA: RWB);
                ^^^ Cannot cast \`roA\` to \`RWB\` because property \`p\` is read-only in \`ROA\` [1] but writable in \`RWB\` [2].
            References:
             17:     declare var roA: ROA;
                                      ^^^ [1]: \`ROA\`
             50: (roA: RWB);
                       ^^^ [2]: \`RWB\`
        `,
      ),
    addCode('(droA: dRWB);')
      .newErrors(
        `
          test.js:52
           52: (droA: dRWB);
                ^^^^ Cannot cast \`droA\` to \`dRWB\` because an indexer property is read-only in \`dROA\` [1] but writable in \`dRWB\` [2].
            References:
             34:     declare var droA: dROA;
                                       ^^^^ [1]: \`dROA\`
             52: (droA: dRWB);
                        ^^^^ [2]: \`dRWB\`

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
                ^^^ Cannot cast \`woA\` to \`RWB\` because property \`p\` is write-only in \`WOA\` [1] but readable in \`RWB\` [2].
            References:
             18:     declare var woA: WOA;
                                      ^^^ [1]: \`WOA\`
             54: (woA: RWB);
                       ^^^ [2]: \`RWB\`
        `,
      ),
    addCode('(dwoA: dRWB);')
      .newErrors(
        `
          test.js:56
           56: (dwoA: dRWB);
                ^^^^ Cannot cast \`dwoA\` to \`dRWB\` because an indexer property is write-only in \`dWOA\` [1] but readable in \`dRWB\` [2].
            References:
             35:     declare var dwoA: dWOA;
                                       ^^^^ [1]: \`dWOA\`
             56: (dwoA: dRWB);
                        ^^^^ [2]: \`dRWB\`
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
                ^^^ Cannot cast \`woA\` to \`ROB\` because property \`p\` is write-only in \`WOA\` [1] but read-only in \`ROB\` [2].
            References:
             18:     declare var woA: WOA;
                                      ^^^ [1]: \`WOA\`
             54: (woA: ROB);
                       ^^^ [2]: \`ROB\`
        `,
      ),
    addCode('(dwoA: dROB);')
      .newErrors(
        `
          test.js:56
           56: (dwoA: dROB);
                ^^^^ Cannot cast \`dwoA\` to \`dROB\` because an indexer property is write-only in \`dWOA\` [1] but read-only in \`dROB\` [2].
            References:
             35:     declare var dwoA: dWOA;
                                       ^^^^ [1]: \`dWOA\`
             56: (dwoA: dROB);
                        ^^^^ [2]: \`dROB\`
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
                ^^^ Cannot cast \`roA\` to \`WOB\` because property \`p\` is read-only in \`ROA\` [1] but write-only in \`WOB\` [2].
            References:
             17:     declare var roA: ROA;
                                      ^^^ [1]: \`ROA\`
             50: (roA: WOB);
                       ^^^ [2]: \`WOB\`
        `,
      ),
    addCode('(droA: dWOB);')
      .newErrors(
        `
          test.js:52
           52: (droA: dWOB);
                ^^^^ Cannot cast \`droA\` to \`dWOB\` because an indexer property is read-only in \`dROA\` [1] but write-only in \`dWOB\` [2].
            References:
             34:     declare var droA: dROA;
                                       ^^^^ [1]: \`dROA\`
             52: (droA: dWOB);
                        ^^^^ [2]: \`dWOB\`
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
                              ^^^^^^ Cannot cast array literal to array type because in type argument \`T\`, property \`p\` is read-only in object type [1] but writable in object type [2].
            References:
             44: (([roA]: Array<{+p:A}>): Array<{p:A}>);
                                ^^^^^^ [1]: object type
             44: (([roA]: Array<{+p:A}>): Array<{p:A}>);
                                                ^^^^^ [2]: object type
        `,
      ),

    addCode('(([woA]: Array<{-p:A}>): Array<{p:A}>);')
      .newErrors(
        `
          test.js:46
           46: (([woA]: Array<{-p:A}>): Array<{p:A}>);
                              ^^^^^^ Cannot cast array literal to array type because in type argument \`T\`, property \`p\` is write-only in object type [1] but readable in object type [2].
            References:
             46: (([woA]: Array<{-p:A}>): Array<{p:A}>);
                                ^^^^^^ [1]: object type
             46: (([woA]: Array<{-p:A}>): Array<{p:A}>);
                                                ^^^^^ [2]: object type
        `,
      ),

    addCode('(([rwA]: Array<{p:A}>): Array<{+p:A}>);')
      .newErrors(
        `
          test.js:48
           48: (([rwA]: Array<{p:A}>): Array<{+p:A}>);
                              ^^^^^ Cannot cast array literal to array type because in type argument \`T\`, property \`p\` is writable in object type [1] but read-only in object type [2].
            References:
             48: (([rwA]: Array<{p:A}>): Array<{+p:A}>);
                                ^^^^^ [1]: object type
             48: (([rwA]: Array<{p:A}>): Array<{+p:A}>);
                                               ^^^^^^ [2]: object type
        `,
      ),

    addCode('(([roA]: Array<{+p:A}>): Array<{+p:A}>);')
      .noNewErrors(),

    addCode('(([woA]: Array<{-p:A}>): Array<{+p:A}>);')
      .newErrors(
        `
          test.js:52
           52: (([woA]: Array<{-p:A}>): Array<{+p:A}>);
                              ^^^^^^ Cannot cast array literal to array type because in type argument \`T\`, property \`p\` is write-only in object type [1] but read-only in object type [2].
            References:
             52: (([woA]: Array<{-p:A}>): Array<{+p:A}>);
                                ^^^^^^ [1]: object type
             52: (([woA]: Array<{-p:A}>): Array<{+p:A}>);
                                                ^^^^^^ [2]: object type
        `,
      ),

    addCode('(([rwA]: Array<{p:A}>): Array<{-p:A}>);')
      .newErrors(
        `
          test.js:54
           54: (([rwA]: Array<{p:A}>): Array<{-p:A}>);
                              ^^^^^ Cannot cast array literal to array type because in type argument \`T\`, property \`p\` is readable in object type [1] but write-only in object type [2].
            References:
             54: (([rwA]: Array<{p:A}>): Array<{-p:A}>);
                                ^^^^^ [1]: object type
             54: (([rwA]: Array<{p:A}>): Array<{-p:A}>);
                                               ^^^^^^ [2]: object type
        `,
      ),

    addCode('(([roA]: Array<{+p:A}>): Array<{-p:A}>);')
      .newErrors(
        `
          test.js:56
           56: (([roA]: Array<{+p:A}>): Array<{-p:A}>);
                              ^^^^^^ Cannot cast array literal to array type because in type argument \`T\`, property \`p\` is read-only in object type [1] but write-only in object type [2].
            References:
             56: (([roA]: Array<{+p:A}>): Array<{-p:A}>);
                                ^^^^^^ [1]: object type
             56: (([roA]: Array<{+p:A}>): Array<{-p:A}>);
                                                ^^^^^^ [2]: object type
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
