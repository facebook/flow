Error ------------------------------------------------------------------------------------------- included/array.js:5:16

Missing an annotation on `x`. [missing-local-annot]

   5|     const a = (x) => 3; // Missing annot
                     ^


Error ------------------------------------------------------------------------------------------ included/array.js:11:18

Missing an annotation on `x`. [missing-local-annot]

   11|       const b = (x) => 3; // Missing annot
                        ^


Error --------------------------------------------------------------------------------------- included/assignment.js:6:6

Cannot assign function to `y` because parameter `x` of unknown type [1] is incompatible with number [2] (consider adding
a type annotation to `x` [1]) in the first parameter. All writes to `y` must be compatible with the type of its
initializer [3]. Add an annotation to `y` [3] if a different type is desired. [incompatible-type]

   included/assignment.js:6:6
   6| y = (x) => 3; // No missing annot
           ^ [1]

References:
   included/assignment.js:5:13
   5| let y = (x: number) => 3;
                  ^^^^^^ [2]
   included/assignment.js:5:5
   5| let y = (x: number) => 3;
          ^ [3]


Error -------------------------------------------------------------------------------------- included/assignment.js:13:6

Missing an annotation on `x`. [missing-local-annot]

   13| c = (x) => 3; // Missing annot
            ^


Error -------------------------------------------------------------------------------------- included/assignment.js:19:6

Missing an annotation on `x`. [missing-local-annot]

   19| e = (x) => 3; // Missing annot
            ^


Error ------------------------------------------------------------------------------------------ included/block.js:14:16

Missing an annotation on `y`. [missing-local-annot]

   14|     const x = (y) => 3;  // error y missing an annotation
                      ^


Error -------------------------------------------------------------------------------------------- included/call.js:9:14

Missing an annotation on `x`. [missing-local-annot]

   9|   const a = (x) => 3; // Required annot
                   ^


Error -------------------------------------------------------------------------------------------- included/call.js:12:8

Property `@@iterator` is missing in function [1] but exists in `$Iterable` [2]. [prop-missing]

   included/call.js:12:8
     12| }, ...((x) => 3));
                ^^^^^^^^ [1]

References:
   <BUILTINS>/core.js:1650:11
   1650| interface $Iterable<+Yield,+Return,-Next> {
                   ^^^^^^^^^ [2]


Error -------------------------------------------------------------------------------------------- included/cast.js:6:15

Missing an annotation on `x`. [missing-local-annot]

   6|   const b = ((x) => 3)(3); // This x does not
                    ^


Error -------------------------------------------------------------------------------------------- included/cast.js:7:14

Missing an annotation on `x`. [missing-local-annot]

   7|   const c = (x) => 3; // Neither does this one
                   ^


Error ------------------------------------------------------------------------------------------- included/cast.js:13:15

Missing an annotation on `x`. [missing-local-annot]

   13|   const b = ((x) => 3)(3); // This x does not
                     ^


Error ------------------------------------------------------------------------------------------- included/cast.js:14:14

Missing an annotation on `x`. [missing-local-annot]

   14|   const c = (x) => 3; // Neither does this one
                    ^


Error ------------------------------------------------------------------------------------------- included/class.js:3:10

Missing an annotation on `param`. [missing-local-annot]

   3|   method(param): any { // error on param
               ^^^^^


Error -------------------------------------------------------------------------------------------- included/class.js:4:6

Missing an annotation on `x`. [missing-local-annot]

   4|     (x) => 3; // error
           ^


Error ------------------------------------------------------------------------------------------- included/class.js:8:16

Missing an annotation on `x`. [missing-local-annot]

   8|   property1 = (x) => 3; // error
                     ^


Error -------------------------------------------------------------------------------- included/class_expression.js:3:10

Missing an annotation on `param`. [missing-local-annot]

   3|   method(param): any { // error on param, annotation is not pushed down
               ^^^^^


Error --------------------------------------------------------------------------------- included/class_expression.js:4:6

Missing an annotation on `x`. [missing-local-annot]

   4|     (x) => 3; // error
           ^


Error -------------------------------------------------------------------------------- included/class_expression.js:8:16

Missing an annotation on `x`. [missing-local-annot]

   8|   property1 = (x) => 3; // error
                     ^


Error ------------------------------------------------------------------------------ included/destructuring_init.js:4:14

Missing an annotation on `x`. [missing-local-annot]

   4|   const d = (x) => 3;
                   ^


Error ------------------------------------------------------------------------------- included/destructuring_init.js:9:8

Property `x` is missing in statics of function type [1]. [prop-missing]

   included/destructuring_init.js:9:8
   9| const {x: b, y: c}: number => void = (x) => { // This x has an annot available
             ^

References:
   included/destructuring_init.js:9:21
   9| const {x: b, y: c}: number => void = (x) => { // This x has an annot available
                          ^^^^^^^^^^^^^^ [1]


Error ------------------------------------------------------------------------------ included/destructuring_init.js:9:14

Property `y` is missing in statics of function type [1]. [prop-missing]

   included/destructuring_init.js:9:14
   9| const {x: b, y: c}: number => void = (x) => { // This x has an annot available
                   ^

References:
   included/destructuring_init.js:9:21
   9| const {x: b, y: c}: number => void = (x) => { // This x has an annot available
                          ^^^^^^^^^^^^^^ [1]


Error ----------------------------------------------------------------------------- included/destructuring_init.js:10:15

Missing an annotation on `x`. [missing-local-annot]

   10|   const b = ((x) => 3)(3); // This x does not
                     ^


Error ----------------------------------------------------------------------------- included/destructuring_init.js:11:14

Missing an annotation on `x`. [missing-local-annot]

   11|   const c = (x) => 3; // Neither does this one
                    ^


Error ----------------------------------------------------------------------------- included/destructuring_init.js:28:14

Missing an annotation on `x`. [missing-local-annot]

   28| [null_1] = [(x) => 1]; // error needs annotation because `null_1` pattern is a provider
                    ^


Error ------------------------------------------------------------------------------ included/destructuring_init.js:33:2

Cannot assign `[...][0]` to `k` because parameter `x` of unknown type [1] is incompatible with string [2] (consider
adding a type annotation to `x` [1]) in the first parameter. All writes to `k` must be compatible with the type of its
initializer [3]. Add an annotation to `k` [3] if a different type is desired. [incompatible-type]

   included/destructuring_init.js:33:2
   33| [k, [l, m]] = [(x) => 1, [(x) => 1, (x) => 1]]; // okay
        ^

References:
   included/destructuring_init.js:33:17
   33| [k, [l, m]] = [(x) => 1, [(x) => 1, (x) => 1]]; // okay
                       ^ [1]
   included/destructuring_init.js:30:13
   30| var k = (x: string) => 1;
                   ^^^^^^ [2]
   included/destructuring_init.js:30:5
   30| var k = (x: string) => 1;
           ^ [3]


Error ------------------------------------------------------------------------------ included/destructuring_init.js:33:6

Cannot assign `[...][1][0]` to `l` because parameter `x` of unknown type [1] is incompatible with string [2] (consider
adding a type annotation to `x` [1]) in the first parameter. All writes to `l` must be compatible with the type of its
initializer [3]. Add an annotation to `l` [3] if a different type is desired. [incompatible-type]

   included/destructuring_init.js:33:6
   33| [k, [l, m]] = [(x) => 1, [(x) => 1, (x) => 1]]; // okay
            ^

References:
   included/destructuring_init.js:33:28
   33| [k, [l, m]] = [(x) => 1, [(x) => 1, (x) => 1]]; // okay
                                  ^ [1]
   included/destructuring_init.js:31:13
   31| var l = (x: string) => 1;
                   ^^^^^^ [2]
   included/destructuring_init.js:31:5
   31| var l = (x: string) => 1;
           ^ [3]


Error ------------------------------------------------------------------------------ included/destructuring_init.js:33:9

Cannot assign `[...][1][1]` to `m` because parameter `x` of unknown type [1] is incompatible with string [2] (consider
adding a type annotation to `x` [1]) in the first parameter. All writes to `m` must be compatible with the type of its
initializer [3]. Add an annotation to `m` [3] if a different type is desired. [incompatible-type]

   included/destructuring_init.js:33:9
   33| [k, [l, m]] = [(x) => 1, [(x) => 1, (x) => 1]]; // okay
               ^

References:
   included/destructuring_init.js:33:38
   33| [k, [l, m]] = [(x) => 1, [(x) => 1, (x) => 1]]; // okay
                                            ^ [1]
   included/destructuring_init.js:32:13
   32| var m = (x: string) => 1;
                   ^^^^^^ [2]
   included/destructuring_init.js:32:5
   32| var m = (x: string) => 1;
           ^ [3]


Error ------------------------------------------------------------------------------ included/destructuring_init.js:38:2

Cannot assign `[...][0]` to `n` because parameter `x` of unknown type [1] is incompatible with string [2] (consider
adding a type annotation to `x` [1]) in the first parameter. All writes to `n` must be compatible with the type of its
initializer [3]. Add an annotation to `n` [3] if a different type is desired. [incompatible-type]

   included/destructuring_init.js:38:2
   38| [n, [o, null_2]] = [
        ^

References:
   included/destructuring_init.js:39:6
   39|     (x) => 1, // TODO should not need annotation
            ^ [1]
   included/destructuring_init.js:35:13
   35| var n = (x: string) => 1;
                   ^^^^^^ [2]
   included/destructuring_init.js:35:5
   35| var n = (x: string) => 1;
           ^ [3]


Error ------------------------------------------------------------------------------ included/destructuring_init.js:38:6

Cannot assign `[...][1][0]` to `o` because parameter `x` of unknown type [1] is incompatible with string [2] (consider
adding a type annotation to `x` [1]) in the first parameter. All writes to `o` must be compatible with the type of its
initializer [3]. Add an annotation to `o` [3] if a different type is desired. [incompatible-type]

   included/destructuring_init.js:38:6
   38| [n, [o, null_2]] = [
            ^

References:
   included/destructuring_init.js:41:10
   41|         (x) => 1, // TODO should not need annotation
                ^ [1]
   included/destructuring_init.js:36:13
   36| var o = (x: string) => 1;
                   ^^^^^^ [2]
   included/destructuring_init.js:36:5
   36| var o = (x: string) => 1;
           ^ [3]


Error ------------------------------------------------------------------------------ included/destructuring_init.js:39:6

Missing an annotation on `x`. [missing-local-annot]

   39|     (x) => 1, // TODO should not need annotation
            ^


Error ----------------------------------------------------------------------------- included/destructuring_init.js:41:10

Missing an annotation on `x`. [missing-local-annot]

   41|         (x) => 1, // TODO should not need annotation
                ^


Error ----------------------------------------------------------------------------- included/destructuring_init.js:42:10

Missing an annotation on `x`. [missing-local-annot]

   42|         (x) => 1, // error needs annotation because null_2 pattern is a provider
                ^


Error ---------------------------------------------------------------------------------------- included/do_while.js:8:16

Missing an annotation on `y`. [missing-local-annot]

   8|     const x = (y) => 3; // non-return, not covered!
                     ^


Error --------------------------------------------------------------------------------------- included/do_while.js:11:11

Missing an annotation on `x`. [missing-local-annot]

   11|   while ((x) => 3); // guard is not covered!
                 ^


Error ---------------------------------------------------------------------------------------- included/do_while.js:12:3

Unreachable code. [unreachable-code]

   12|   return (y) => 3;
         ^^^^^^^^^^^^^^^^


Error ---------------------------------------------------------------------------------------------- included/for.js:7:9

Missing an annotation on `x`. [missing-local-annot]

   7|   for ((x) => true; (x) => true; (x) => true) { // init, test, and update are all not covered by the annot 
              ^


Error --------------------------------------------------------------------------------------------- included/for.js:7:22

Missing an annotation on `x`. [missing-local-annot]

   7|   for ((x) => true; (x) => true; (x) => true) { // init, test, and update are all not covered by the annot 
                           ^


Error --------------------------------------------------------------------------------------------- included/for.js:7:35

Missing an annotation on `x`. [missing-local-annot]

   7|   for ((x) => true; (x) => true; (x) => true) { // init, test, and update are all not covered by the annot 
                                        ^


Error --------------------------------------------------------------------------------------------- included/for.js:8:16

Missing an annotation on `y`. [missing-local-annot]

   8|     const x = (y) => 3; // non-return, not covered!
                     ^


Error ------------------------------------------------------------------------------------------ included/for_in.js:7:19

Missing an annotation on `x`. [missing-local-annot]

   7|   for (let x in ((x) => 3)) { // Only the body is covered, missing annot!
                        ^


Error ------------------------------------------------------------------------------------------ included/for_in.js:8:16

Missing an annotation on `y`. [missing-local-annot]

   8|     const x = (y) => 3; // non-return, not covered!
                     ^


Error ------------------------------------------------------------------------------------------ included/for_of.js:7:18

Property `@@iterator` is missing in function [1] but exists in `$Iterable` [2]. [prop-missing]

   included/for_of.js:7:18
      7|   for (let x of ((x) => 3)) { // Only the body is covered, missing annot! Also a type error
                          ^^^^^^^^ [1]

References:
   <BUILTINS>/core.js:1650:11
   1650| interface $Iterable<+Yield,+Return,-Next> {
                   ^^^^^^^^^ [2]


Error ------------------------------------------------------------------------------------------ included/for_of.js:7:19

Missing an annotation on `x`. [missing-local-annot]

   7|   for (let x of ((x) => 3)) { // Only the body is covered, missing annot! Also a type error
                        ^


Error ------------------------------------------------------------------------------------------ included/for_of.js:8:16

Missing an annotation on `y`. [missing-local-annot]

   8|     const x = (y) => 3; // non-return, not covered!
                     ^


Error ----------------------------------------------------------------------------------------- included/id_init.js:6:15

Missing an annotation on `x`. [missing-local-annot]

   6|   const b = ((x) => 3)(3); // This x does not
                    ^


Error ----------------------------------------------------------------------------------------- included/id_init.js:7:14

Missing an annotation on `x`. [missing-local-annot]

   7|   const c = (x) => 3; // Neither does this one
                   ^


Error ----------------------------------------------------------------------------------------------- included/if.js:7:9

Missing an annotation on `x`. [missing-local-annot]

   7|   if (((x) => true) ()){ // Guards are not covered by the annot
              ^


Error --------------------------------------------------------------------------------------------- included/if.js:10:16

Missing an annotation on `x`. [missing-local-annot]

   10|     const z = (x) => 3; // Non-return statements are not covered by the annot
                      ^


Error --------------------------------------------------------------------------------------------- included/jsx.js:9:14

Missing an annotation on `x`. [missing-local-annot]

   9|   const y = (x) => 3; // error, required annot
                   ^


Error --------------------------------------------------------------------------------------- included/no_annots.js:3:12

Missing an annotation on `x`. [missing-local-annot]

   3| function f(x): void {}
                 ^


Error --------------------------------------------------------------------------------------- included/no_annots.js:4:32

Missing an annotation on `x`. [missing-local-annot]

   4| const function_expr = function(x): void {};
                                     ^


Error --------------------------------------------------------------------------------------- included/no_annots.js:5:21

Missing an annotation on `x`. [missing-local-annot]

   5| const arrow_expr = (x) => {};
                          ^


Error --------------------------------------------------------------------------------------- included/no_annots.js:6:49

Missing an annotation on `x`. [missing-local-annot]

   6| const deep_function_expr = {f: {g: {h: function(x): void {}}}};
                                                      ^


Error --------------------------------------------------------------------------------------- included/no_annots.js:7:38

Missing an annotation on `x`. [missing-local-annot]

   7| const deep_arrow_expr = {f: {g: {h: (x) => void {}}}};
                                           ^


Error ------------------------------------------------------------------------------------------ included/object.js:5:16

Missing an annotation on `x`. [missing-local-annot]

   5|     const a = (x) => 3; // Missing annot
                     ^


Error ------------------------------------------------------------------------------------------ included/object.js:9:16

Missing an annotation on `x`. [missing-local-annot]

   9|     const b = (x) => 3; // Missing annot
                     ^


Error ----------------------------------------------------------------------------------------- included/object.js:16:15

Missing an annotation on `x`. [missing-local-annot]

   16|   __proto__: (x) => {
                     ^


Error ----------------------------------------------------------------------------------------- included/object.js:17:16

Missing an annotation on `x`. [missing-local-annot]

   17|     const c = (x) => 3; // Missing annot
                      ^


Error ----------------------------------------------------------------------------------------- included/object.js:18:13

Missing an annotation on `x`. [missing-local-annot]

   18|     return (x) => 3;
                   ^


Error ----------------------------------------------------------------------------------------- included/object.js:21:16

Missing an annotation on `x`. [missing-local-annot]

   21|     const d = (x) => 3; // Missing annot
                      ^


Error ----------------------------------------------------------------------------------------- included/object.js:26:16

Missing an annotation on `x`. [missing-local-annot]

   26|     const e = (x) => 3; // Missing annot
                      ^


Error ----------------------------------------------------------------------------------------- included/object.js:30:16

Missing an annotation on `x`. [missing-local-annot]

   30|     const f = (x) => 3; // Missing annot
                      ^


Error ----------------------------------------------------------------------------------------- included/object.js:34:16

Missing an annotation on `x`. [missing-local-annot]

   34|     const g = (x) => 3; // Missing annot
                      ^


Error ----------------------------------------------------------------------------------------- included/object.js:38:16

Missing an annotation on `x`. [missing-local-annot]

   38|     const h = (x) => 3; // Missing annot
                      ^


Error ------------------------------------------------------------------------------------------ included/return.js:7:22

Missing an annotation on `z`. [missing-local-annot]

   7| let b = ((x => y => (z => 3)(3)): number => number => number); // annot missing on z
                           ^


Error ----------------------------------------------------------------------------------------- included/return.js:11:40

Missing an annotation on `z`. [missing-local-annot]

   11| let d = ((x => { return y => { return (z => 3)(3) }}): number => number => number); // annot missing on z
                                              ^


Error ----------------------------------------------------------------------------------------- included/return.js:15:15

Missing an annotation on `x`. [missing-local-annot]

   15|   const xx = (x) => 3; // Missing annot on x
                     ^


Error ----------------------------------------------------------------------------------------- included/return.js:17:16

Missing an annotation on `x`. [missing-local-annot]

   17|     const z = (x) => 3; // Missing annot on x
                      ^


Error ------------------------------------------------------------------------------------------ included/switch.js:7:12

Missing an annotation on `x`. [missing-local-annot]

   7|   switch ((x) => true){ // Guards are not covered by the annot
                 ^


Error ----------------------------------------------------------------------------------------- included/switch.js:11:18

Missing an annotation on `x`. [missing-local-annot]

   11|       const z = (x) => 3; // Non-return statements are not covered by the annot
                        ^


Error -------------------------------------------------------------------------------------------- included/this.js:2:13

Missing an annotation on implicit `this` parameter of function. [missing-this-annot]

   2| function foo() {return this.x;}
                  ^^


Error ------------------------------------------------------------------------------------------- included/this.js:18:18

Missing an annotation on implicit `this` parameter of function. [missing-this-annot]

   18| let f = function () { return this.x }
                        ^^


Error ------------------------------------------------------------------------------- included/try_catch_finally.js:8:16

Missing an annotation on `x`. [missing-local-annot]

   8|     const z = (x) => 3; // Non-return statements are not covered by the annot
                     ^


Error ------------------------------------------------------------------------------ included/try_catch_finally.js:11:16

Missing an annotation on `x`. [missing-local-annot]

   11|     const z = (x) => 3; // Non-return statements are not covered by the annot
                      ^


Error ------------------------------------------------------------------------------ included/try_catch_finally.js:14:16

Missing an annotation on `x`. [missing-local-annot]

   14|     const z = (x) => 3; // Non-return statements are not covered by the annot
                      ^


Error ------------------------------------------------------------------------------------------- included/while.js:7:11

Missing an annotation on `x`. [missing-local-annot]

   7|   while ((x) => 3) { // Only the body is covered, missing annot!
                ^


Error ------------------------------------------------------------------------------------------- included/while.js:8:16

Missing an annotation on `y`. [missing-local-annot]

   8|     const x = (y) => 3; // non-return, not covered!
                     ^



Found 77 errors
