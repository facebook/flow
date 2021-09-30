/*
 * @flow
 */

import type {Suite} from "flow-dev-tools/src/test/Suite";
const {suite, test} = require('flow-dev-tools/src/test/Tester');

module.exports = (suite(({addFile, addFiles, addCode}) => [
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
                ^^^ Cannot cast \`roA\` to \`RWA\` because property \`p\` is read-only in \`ROA\` [1] but writable in \`RWA\` [2]. [incompatible-variance]
            References:
             17:     declare var roA: ROA;
                                      ^^^ [1]
             50: (roA: RWA);
                       ^^^ [2]
        `,
      ),
    addCode('(droA: dRWA);')
      .newErrors(
        `
          test.js:52
           52: (droA: dRWA);
                ^^^^ Cannot cast \`droA\` to \`dRWA\` because an index signature declaring the expected key / value type is read-only in \`dROA\` [1] but writable in \`dRWA\` [2]. [incompatible-variance]
            References:
             34:     declare var droA: dROA;
                                       ^^^^ [1]
             52: (droA: dRWA);
                        ^^^^ [2]
        `,
      ),

    // -A
    addCode('(woA: RWA);')
      .newErrors(
        `
          test.js:54
           54: (woA: RWA);
                ^^^ Cannot cast \`woA\` to \`RWA\` because property \`p\` is write-only in \`WOA\` [1] but readable in \`RWA\` [2]. [incompatible-variance]
            References:
             18:     declare var woA: WOA;
                                      ^^^ [1]
             54: (woA: RWA);
                       ^^^ [2]
        `,
      ),
    addCode('(dwoA: dRWA);')
      .newErrors(
        `
          test.js:56
           56: (dwoA: dRWA);
                ^^^^ Cannot cast \`dwoA\` to \`dRWA\` because an index signature declaring the expected key / value type is write-only in \`dWOA\` [1] but readable in \`dRWA\` [2]. [incompatible-variance]
            References:
             35:     declare var dwoA: dWOA;
                                       ^^^^ [1]
             56: (dwoA: dRWA);
                        ^^^^ [2]
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
                ^^^ Cannot cast \`rwB\` to \`RWA\` because \`B\` [1] is incompatible with \`A\` [2] in property \`p\`. [incompatible-cast]
            References:
             12:     type RWB = {|p: B|}
                                     ^ [1]
              9:     type RWA = {|p: A|}
                                     ^ [2]
        `,
      ),
    addCode('(drwB: dRWA);')
      .newErrors(
        `
          test.js:64
           64: (drwB: dRWA);
                ^^^^ Cannot cast \`drwB\` to \`dRWA\` because \`B\` [1] is incompatible with \`A\` [2] in the indexer property. [incompatible-cast]
            References:
             29:     type dRWB = {[string]: B};
                                            ^ [1]
             26:     type dRWA = {[string]: A};
                                            ^ [2]
        `,
      ),

    // +B
    addCode('(roB: RWA);')
      .newErrors(
        `
          test.js:66
           66: (roB: RWA);
                ^^^ Cannot cast \`roB\` to \`RWA\` because property \`p\` is read-only in \`ROB\` [1] but writable in \`RWA\` [2]. [incompatible-variance]
            References:
             21:     declare var roB: ROB;
                                      ^^^ [1]
             66: (roB: RWA);
                       ^^^ [2]
        `,
      ),
    addCode('(droB: dRWA);')
      .newErrors(
        `
          test.js:68
           68: (droB: dRWA);
                ^^^^ Cannot cast \`droB\` to \`dRWA\` because an index signature declaring the expected key / value type is read-only in \`dROB\` [1] but writable in \`dRWA\` [2]. [incompatible-variance]
            References:
             38:     declare var droB: dROB;
                                       ^^^^ [1]
             68: (droB: dRWA);
                        ^^^^ [2]
        `,
      ),

    // -B
    addCode('(woB: RWA);')
      .newErrors(
        `
          test.js:70
           70: (woB: RWA);
                ^^^ Cannot cast \`woB\` to \`RWA\` because property \`p\` is write-only in \`WOB\` [1] but readable in \`RWA\` [2]. [incompatible-variance]
            References:
             22:     declare var woB: WOB;
                                      ^^^ [1]
             70: (woB: RWA);
                       ^^^ [2]

          test.js:70
           70: (woB: RWA);
                ^^^ Cannot cast \`woB\` to \`RWA\` because \`A\` [1] is incompatible with \`B\` [2] in property \`p\`. [incompatible-cast]
            References:
              9:     type RWA = {|p: A|}
                                     ^ [1]
             14:     type WOB = {|-p: B|}
                                      ^ [2]
        `,
      ),
    addCode('(dwoB: dRWA);')
      .newErrors(
        `
          test.js:72
           72: (dwoB: dRWA);
                ^^^^ Cannot cast \`dwoB\` to \`dRWA\` because an index signature declaring the expected key / value type is write-only in \`dWOB\` [1] but readable in \`dRWA\` [2]. [incompatible-variance]
            References:
             39:     declare var dwoB: dWOB;
                                       ^^^^ [1]
             72: (dwoB: dRWA);
                        ^^^^ [2]

          test.js:72
           72: (dwoB: dRWA);
                ^^^^ Cannot cast \`dwoB\` to \`dRWA\` because \`A\` [1] is incompatible with \`B\` [2] in the indexer property. [incompatible-cast]
            References:
             26:     type dRWA = {[string]: A};
                                            ^ [1]
             31:     type dWOB = {-[string]: B};
                                             ^ [2]
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
                ^^^ Cannot cast \`woA\` to \`ROA\` because property \`p\` is write-only in \`WOA\` [1] but read-only in \`ROA\` [2]. [incompatible-variance]
            References:
             18:     declare var woA: WOA;
                                      ^^^ [1]
             54: (woA: ROA);
                       ^^^ [2]
        `,
      ),
    addCode('(dwoA: dROA);')
      .newErrors(
        `
          test.js:56
           56: (dwoA: dROA);
                ^^^^ Cannot cast \`dwoA\` to \`dROA\` because an index signature declaring the expected key / value type is write-only in \`dWOA\` [1] but read-only in \`dROA\` [2]. [incompatible-variance]
            References:
             35:     declare var dwoA: dWOA;
                                       ^^^^ [1]
             56: (dwoA: dROA);
                        ^^^^ [2]
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
                ^^^ Cannot cast \`woB\` to \`ROA\` because property \`p\` is write-only in \`WOB\` [1] but read-only in \`ROA\` [2]. [incompatible-variance]
            References:
             22:     declare var woB: WOB;
                                      ^^^ [1]
             70: (woB: ROA);
                       ^^^ [2]
        `,
      ),
    addCode('(dwoB: dROA);')
      .newErrors(
        `
          test.js:72
           72: (dwoB: dROA);
                ^^^^ Cannot cast \`dwoB\` to \`dROA\` because an index signature declaring the expected key / value type is write-only in \`dWOB\` [1] but read-only in \`dROA\` [2]. [incompatible-variance]
            References:
             39:     declare var dwoB: dWOB;
                                       ^^^^ [1]
             72: (dwoB: dROA);
                        ^^^^ [2]
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
                ^^^ Cannot cast \`roA\` to \`WOA\` because property \`p\` is read-only in \`ROA\` [1] but write-only in \`WOA\` [2]. [incompatible-variance]
            References:
             17:     declare var roA: ROA;
                                      ^^^ [1]
             50: (roA: WOA);
                       ^^^ [2]
        `,
      ),
    addCode('(droA: dWOA);')
      .newErrors(
        `
          test.js:52
           52: (droA: dWOA);
                ^^^^ Cannot cast \`droA\` to \`dWOA\` because an index signature declaring the expected key / value type is read-only in \`dROA\` [1] but write-only in \`dWOA\` [2]. [incompatible-variance]
            References:
             34:     declare var droA: dROA;
                                       ^^^^ [1]
             52: (droA: dWOA);
                        ^^^^ [2]
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
                ^^^ Cannot cast \`rwB\` to \`WOA\` because \`A\` [1] is incompatible with \`B\` [2] in property \`p\`. [incompatible-cast]
            References:
             11:     type WOA = {|-p: A|}
                                      ^ [1]
             12:     type RWB = {|p: B|}
                                     ^ [2]
        `,
      ),
    addCode('(drwB: dWOA);')
      .newErrors(
        `
          test.js:64
           64: (drwB: dWOA);
                ^^^^ Cannot cast \`drwB\` to \`dWOA\` because \`A\` [1] is incompatible with \`B\` [2] in the indexer property. [incompatible-cast]
            References:
             28:     type dWOA = {-[string]: A};
                                             ^ [1]
             29:     type dRWB = {[string]: B};
                                            ^ [2]
        `,
      ),

    // +B
    addCode('(roB: WOA);')
      .newErrors(
        `
          test.js:66
           66: (roB: WOA);
                ^^^ Cannot cast \`roB\` to \`WOA\` because property \`p\` is read-only in \`ROB\` [1] but write-only in \`WOA\` [2]. [incompatible-variance]
            References:
             21:     declare var roB: ROB;
                                      ^^^ [1]
             66: (roB: WOA);
                       ^^^ [2]
        `,
      ),
    addCode('(droB: dWOA);')
      .newErrors(
        `
          test.js:68
           68: (droB: dWOA);
                ^^^^ Cannot cast \`droB\` to \`dWOA\` because an index signature declaring the expected key / value type is read-only in \`dROB\` [1] but write-only in \`dWOA\` [2]. [incompatible-variance]
            References:
             38:     declare var droB: dROB;
                                       ^^^^ [1]
             68: (droB: dWOA);
                        ^^^^ [2]
        `,
      ),

    // -B
    addCode('(woB: WOA);')
      .newErrors(
        `
          test.js:70
           70: (woB: WOA);
                ^^^ Cannot cast \`woB\` to \`WOA\` because \`A\` [1] is incompatible with \`B\` [2] in property \`p\`. [incompatible-cast]
            References:
             11:     type WOA = {|-p: A|}
                                      ^ [1]
             14:     type WOB = {|-p: B|}
                                      ^ [2]
        `,
      ),
    addCode('(dwoB: dWOA);')
      .newErrors(
        `
          test.js:72
           72: (dwoB: dWOA);
                ^^^^ Cannot cast \`dwoB\` to \`dWOA\` because \`A\` [1] is incompatible with \`B\` [2] in the indexer property. [incompatible-cast]
            References:
             28:     type dWOA = {-[string]: A};
                                             ^ [1]
             31:     type dWOB = {-[string]: B};
                                             ^ [2]
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
                    ^^^^^ Cannot cast object literal to \`RWB\` because \`A\` [1] is incompatible with \`B\` [2] in property \`p\`. [incompatible-cast]
            References:
             42: ({p: new A}: RWB);
                      ^^^^^ [1]
             12:     type RWB = {|p: B|}
                                     ^ [2]
        `,
      ),
    addCode('({p: new A}: dRWB);')
      .newErrors(
        `
          test.js:44
           44: ({p: new A}: dRWB);
                    ^^^^^ Cannot cast object literal to \`dRWB\` because \`A\` [1] is incompatible with \`B\` [2] in property \`p\`. [incompatible-cast]
            References:
             44: ({p: new A}: dRWB);
                      ^^^^^ [1]
             29:     type dRWB = {[string]: B};
                                            ^ [2]
        `,
      ),

    // A
    addCode('(rwA: RWB);')
      .newErrors(
        `
          test.js:46
           46: (rwA: RWB);
                ^^^ Cannot cast \`rwA\` to \`RWB\` because \`A\` [1] is incompatible with \`B\` [2] in property \`p\`. [incompatible-cast]
            References:
              9:     type RWA = {|p: A|}
                                     ^ [1]
             12:     type RWB = {|p: B|}
                                     ^ [2]
        `,
      ),
    addCode('(drwA: dRWB);')
      .newErrors(
        `
          test.js:48
           48: (drwA: dRWB);
                ^^^^ Cannot cast \`drwA\` to \`dRWB\` because \`A\` [1] is incompatible with \`B\` [2] in the indexer property. [incompatible-cast]
            References:
             26:     type dRWA = {[string]: A};
                                            ^ [1]
             29:     type dRWB = {[string]: B};
                                            ^ [2]
        `,
      ),

    // +A
    addCode('(roA: RWB);')
      .newErrors(
        `
          test.js:50
           50: (roA: RWB);
                ^^^ Cannot cast \`roA\` to \`RWB\` because property \`p\` is read-only in \`ROA\` [1] but writable in \`RWB\` [2]. [incompatible-variance]
            References:
             17:     declare var roA: ROA;
                                      ^^^ [1]
             50: (roA: RWB);
                       ^^^ [2]

          test.js:50
           50: (roA: RWB);
                ^^^ Cannot cast \`roA\` to \`RWB\` because \`A\` [1] is incompatible with \`B\` [2] in property \`p\`. [incompatible-cast]
            References:
             10:     type ROA = {|+p: A|}
                                      ^ [1]
             12:     type RWB = {|p: B|}
                                     ^ [2]
        `,
      ),
    addCode('(droA: dRWB);')
      .newErrors(
        `
          test.js:52
           52: (droA: dRWB);
                ^^^^ Cannot cast \`droA\` to \`dRWB\` because an index signature declaring the expected key / value type is read-only in \`dROA\` [1] but writable in \`dRWB\` [2]. [incompatible-variance]
            References:
             34:     declare var droA: dROA;
                                       ^^^^ [1]
             52: (droA: dRWB);
                        ^^^^ [2]

          test.js:52
           52: (droA: dRWB);
                ^^^^ Cannot cast \`droA\` to \`dRWB\` because \`A\` [1] is incompatible with \`B\` [2] in the indexer property. [incompatible-cast]
            References:
             27:     type dROA = {+[string]: A};
                                             ^ [1]
             29:     type dRWB = {[string]: B};
                                            ^ [2]
        `,
      ),

    // -A
    addCode('(woA: RWB);')
      .newErrors(
        `
          test.js:54
           54: (woA: RWB);
                ^^^ Cannot cast \`woA\` to \`RWB\` because property \`p\` is write-only in \`WOA\` [1] but readable in \`RWB\` [2]. [incompatible-variance]
            References:
             18:     declare var woA: WOA;
                                      ^^^ [1]
             54: (woA: RWB);
                       ^^^ [2]
        `,
      ),
    addCode('(dwoA: dRWB);')
      .newErrors(
        `
          test.js:56
           56: (dwoA: dRWB);
                ^^^^ Cannot cast \`dwoA\` to \`dRWB\` because an index signature declaring the expected key / value type is write-only in \`dWOA\` [1] but readable in \`dRWB\` [2]. [incompatible-variance]
            References:
             35:     declare var dwoA: dWOA;
                                       ^^^^ [1]
             56: (dwoA: dRWB);
                        ^^^^ [2]
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
                    ^^^^^ Cannot cast object literal to \`ROB\` because \`A\` [1] is incompatible with \`B\` [2] in property \`p\`. [incompatible-cast]
            References:
             42: ({p: new A}: ROB);
                      ^^^^^ [1]
             13:     type ROB = {|+p: B|}
                                      ^ [2]
        `,
      ),
    addCode('({p: new A}: dROB);')
      .newErrors(
        `
          test.js:44
           44: ({p: new A}: dROB);
                    ^^^^^ Cannot cast object literal to \`dROB\` because \`A\` [1] is incompatible with \`B\` [2] in property \`p\`. [incompatible-cast]
            References:
             44: ({p: new A}: dROB);
                      ^^^^^ [1]
             30:     type dROB = {+[string]: B};
                                             ^ [2]
        `,
      ),

    // A
    addCode('(rwA: ROB);')
      .newErrors(
        `
          test.js:46
           46: (rwA: ROB);
                ^^^ Cannot cast \`rwA\` to \`ROB\` because \`A\` [1] is incompatible with \`B\` [2] in property \`p\`. [incompatible-cast]
            References:
              9:     type RWA = {|p: A|}
                                     ^ [1]
             13:     type ROB = {|+p: B|}
                                      ^ [2]
        `,
      ),
    addCode('(drwA: dROB);')
      .newErrors(
        `
          test.js:48
           48: (drwA: dROB);
                ^^^^ Cannot cast \`drwA\` to \`dROB\` because \`A\` [1] is incompatible with \`B\` [2] in the indexer property. [incompatible-cast]
            References:
             26:     type dRWA = {[string]: A};
                                            ^ [1]
             30:     type dROB = {+[string]: B};
                                             ^ [2]
        `,
      ),

    // +A
    addCode('(roA: ROB);')
      .newErrors(
        `
          test.js:50
           50: (roA: ROB);
                ^^^ Cannot cast \`roA\` to \`ROB\` because \`A\` [1] is incompatible with \`B\` [2] in property \`p\`. [incompatible-cast]
            References:
             10:     type ROA = {|+p: A|}
                                      ^ [1]
             13:     type ROB = {|+p: B|}
                                      ^ [2]
        `,
      ),
    addCode('(droA: dROB);')
      .newErrors(
        `
          test.js:52
           52: (droA: dROB);
                ^^^^ Cannot cast \`droA\` to \`dROB\` because \`A\` [1] is incompatible with \`B\` [2] in the indexer property. [incompatible-cast]
            References:
             27:     type dROA = {+[string]: A};
                                             ^ [1]
             30:     type dROB = {+[string]: B};
                                             ^ [2]
        `,
      ),

    // -A
    addCode('(woA: ROB);')
      .newErrors(
        `
          test.js:54
           54: (woA: ROB);
                ^^^ Cannot cast \`woA\` to \`ROB\` because property \`p\` is write-only in \`WOA\` [1] but read-only in \`ROB\` [2]. [incompatible-variance]
            References:
             18:     declare var woA: WOA;
                                      ^^^ [1]
             54: (woA: ROB);
                       ^^^ [2]
        `,
      ),
    addCode('(dwoA: dROB);')
      .newErrors(
        `
          test.js:56
           56: (dwoA: dROB);
                ^^^^ Cannot cast \`dwoA\` to \`dROB\` because an index signature declaring the expected key / value type is write-only in \`dWOA\` [1] but read-only in \`dROB\` [2]. [incompatible-variance]
            References:
             35:     declare var dwoA: dWOA;
                                       ^^^^ [1]
             56: (dwoA: dROB);
                        ^^^^ [2]
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
                ^^^ Cannot cast \`roA\` to \`WOB\` because property \`p\` is read-only in \`ROA\` [1] but write-only in \`WOB\` [2]. [incompatible-variance]
            References:
             17:     declare var roA: ROA;
                                      ^^^ [1]
             50: (roA: WOB);
                       ^^^ [2]
        `,
      ),
    addCode('(droA: dWOB);')
      .newErrors(
        `
          test.js:52
           52: (droA: dWOB);
                ^^^^ Cannot cast \`droA\` to \`dWOB\` because an index signature declaring the expected key / value type is read-only in \`dROA\` [1] but write-only in \`dWOB\` [2]. [incompatible-variance]
            References:
             34:     declare var droA: dROA;
                                       ^^^^ [1]
             52: (droA: dWOB);
                        ^^^^ [2]
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
                              ^^^^^^ Cannot cast array literal to array type because property \`p\` is read-only in object type [1] but writable in object type [2] in array element. [incompatible-variance]
            References:
             44: (([roA]: Array<{+p:A}>): Array<{p:A}>);
                                ^^^^^^ [1]
             44: (([roA]: Array<{+p:A}>): Array<{p:A}>);
                                                ^^^^^ [2]
        `,
      ),

    addCode('(([woA]: Array<{-p:A}>): Array<{p:A}>);')
      .newErrors(
        `
          test.js:46
           46: (([woA]: Array<{-p:A}>): Array<{p:A}>);
                              ^^^^^^ Cannot cast array literal to array type because property \`p\` is write-only in object type [1] but readable in object type [2] in array element. [incompatible-variance]
            References:
             46: (([woA]: Array<{-p:A}>): Array<{p:A}>);
                                ^^^^^^ [1]
             46: (([woA]: Array<{-p:A}>): Array<{p:A}>);
                                                ^^^^^ [2]
        `,
      ),

    addCode('(([rwA]: Array<{p:A}>): Array<{+p:A}>);')
      .newErrors(
        `
          test.js:48
           48: (([rwA]: Array<{p:A}>): Array<{+p:A}>);
                              ^^^^^ Cannot cast array literal to array type because property \`p\` is writable in object type [1] but read-only in object type [2] in array element. [incompatible-variance]
            References:
             48: (([rwA]: Array<{p:A}>): Array<{+p:A}>);
                                ^^^^^ [1]
             48: (([rwA]: Array<{p:A}>): Array<{+p:A}>);
                                               ^^^^^^ [2]
        `,
      ),

    addCode('(([roA]: Array<{+p:A}>): Array<{+p:A}>);')
      .noNewErrors(),

    addCode('(([woA]: Array<{-p:A}>): Array<{+p:A}>);')
      .newErrors(
        `
          test.js:52
           52: (([woA]: Array<{-p:A}>): Array<{+p:A}>);
                              ^^^^^^ Cannot cast array literal to array type because property \`p\` is write-only in object type [1] but read-only in object type [2] in array element. [incompatible-variance]
            References:
             52: (([woA]: Array<{-p:A}>): Array<{+p:A}>);
                                ^^^^^^ [1]
             52: (([woA]: Array<{-p:A}>): Array<{+p:A}>);
                                                ^^^^^^ [2]
        `,
      ),

    addCode('(([rwA]: Array<{p:A}>): Array<{-p:A}>);')
      .newErrors(
        `
          test.js:54
           54: (([rwA]: Array<{p:A}>): Array<{-p:A}>);
                              ^^^^^ Cannot cast array literal to array type because property \`p\` is readable in object type [1] but write-only in object type [2] in array element. [incompatible-variance]
            References:
             54: (([rwA]: Array<{p:A}>): Array<{-p:A}>);
                                ^^^^^ [1]
             54: (([rwA]: Array<{p:A}>): Array<{-p:A}>);
                                               ^^^^^^ [2]
        `,
      ),

    addCode('(([roA]: Array<{+p:A}>): Array<{-p:A}>);')
      .newErrors(
        `
          test.js:56
           56: (([roA]: Array<{+p:A}>): Array<{-p:A}>);
                              ^^^^^^ Cannot cast array literal to array type because property \`p\` is read-only in object type [1] but write-only in object type [2] in array element. [incompatible-variance]
            References:
             56: (([roA]: Array<{+p:A}>): Array<{-p:A}>);
                                ^^^^^^ [1]
             56: (([roA]: Array<{+p:A}>): Array<{-p:A}>);
                                                ^^^^^^ [2]
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
               ^^^ Cannot cast \`woA\` to \`RWA\` because property \`p\` is not readable. [cannot-read]
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
               ^^^ Cannot cast \`woB\` to \`RWA\` because property \`p\` is not readable. [cannot-read]
       `,
     ),

    // A ~> B
    addCode('(rwA: $Shape<RWB>);').
      newErrors(
       `
         test.js:54
          54: (rwA: $Shape<RWB>);
               ^^^ Cannot cast \`rwA\` to \`RWB\` because \`A\` [1] is incompatible with \`B\` [2] in property \`p\`. [incompatible-cast]
           References:
             9:     type RWA = {|p: A|}
                                    ^ [1]
            12:     type RWB = {|p: B|}
                                    ^ [2]
       `,
     ),

    // +A ~> B
    addCode('(roA: $Shape<RWB>);').
      newErrors(
       `
         test.js:56
          56: (roA: $Shape<RWB>);
               ^^^ Cannot cast \`roA\` to \`RWB\` because \`A\` [1] is incompatible with \`B\` [2] in property \`p\`. [incompatible-cast]
           References:
            10:     type ROA = {|+p: A|}
                                     ^ [1]
            12:     type RWB = {|p: B|}
                                    ^ [2]
       `,
     ),

    // -A ~> B
    addCode('(woA: $Shape<RWB>);').
      newErrors(
       `
         test.js:58
          58: (woA: $Shape<RWB>);
               ^^^ Cannot cast \`woA\` to \`RWB\` because property \`p\` is not readable. [cannot-read]
       `,
     ),
  ]),

]).beforeEach(({addCode}) => [
  addCode(`
    class A {}
    class B extends A {};

    // named properties

    type RWA = {|p: A|}
    type ROA = {|+p: A|}
    type WOA = {|-p: A|}
    type RWB = {|p: B|}
    type ROB = {|+p: B|}
    type WOB = {|-p: B|}

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
]): Suite);
