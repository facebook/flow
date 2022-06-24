Error ------------------------------------------------------------------------------------- crash_regression_test.js:7:7

Cannot declare `x` [1] because the name is already bound. [name-already-bound]

   crash_regression_test.js:7:7
   7|   let x: number = 0; // Error
            ^

References:
   crash_regression_test.js:6:7
   6|   let x = 0;
            ^ [1]


Error ----------------------------------------------------------------------------------- crash_regression_test.js:11:12

Cannot declare `y` [1] because the name is already bound. [name-already-bound]

   crash_regression_test.js:11:12
   11|   function y() {} // Error
                  ^

References:
   crash_regression_test.js:10:7
   10|   let y = 0;
             ^ [1]


Error ------------------------------------------------------------------------------------------------------ test.js:4:5

Cannot declare `f1` [1] because the name is already bound. [name-already-bound]

   test.js:4:5
   4| var f1: number = 1; // error [name-already-bound]
          ^^

References:
   test.js:3:10
   3| function f1(): void {}
               ^^ [1]


Error ------------------------------------------------------------------------------------------------------ test.js:7:5

Cannot declare `f2` [1] because the name is already bound. [name-already-bound]

   test.js:7:5
   7| let f2: number = 2; // error [name-already-bound]
          ^^

References:
   test.js:6:10
   6| function f2(): void {}
               ^^ [1]


Error ----------------------------------------------------------------------------------------------------- test.js:10:7

Cannot declare `f3` [1] because the name is already bound. [name-already-bound]

   test.js:10:7
   10| const f3: number = 3; // error [name-already-bound]
             ^^

References:
   test.js:9:10
    9| function f3(): void {}
                ^^ [1]


Error ----------------------------------------------------------------------------------------------------- test.js:13:5

Cannot declare `f4` [1] because the name is already bound. [name-already-bound]

   test.js:13:5
   13| var f4 = 4; // error [name-already-bound]
           ^^

References:
   test.js:12:10
   12| function f4(): void {}
                ^^ [1]


Error ----------------------------------------------------------------------------------------------------- test.js:19:5

Cannot declare `f5` [1] because the name is already bound. [name-already-bound]

   test.js:19:5
   19| var f5 = 5; // error [name-already-bound]
           ^^

References:
   test.js:15:18
   15| declare function f5(): void;
                        ^^ [1]


Error ----------------------------------------------------------------------------------------------------- test.js:22:5

Cannot declare `c1` [1] because the name is already bound. [name-already-bound]

   test.js:22:5
   22| var c1: number = 1; // error [name-already-bound]
           ^^

References:
   test.js:21:7
   21| class c1 {}
             ^^ [1]


Error ----------------------------------------------------------------------------------------------------- test.js:25:5

Cannot declare `c2` [1] because the name is already bound. [name-already-bound]

   test.js:25:5
   25| var c2 = 2; // error [name-already-bound]
           ^^

References:
   test.js:24:7
   24| class c2 {}
             ^^ [1]



Found 9 errors
