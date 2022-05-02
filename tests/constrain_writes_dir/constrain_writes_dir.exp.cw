Error --------------------------------------------------------------------------------------------- excluded/test.js:4:5

Cannot assign `"true"` to `x` because string [1] is incompatible with number [2]. All writes to `x` must be compatible
with the type of its initializer [3]. Add an annotation to `x` [3] if a different type is desired. [incompatible-type]

   excluded/test.js:4:5
   4| x = "true";
          ^^^^^^ [1]

References:
   excluded/test.js:3:9
   3| var x = 42;
              ^^ [2]
   excluded/test.js:3:5
   3| var x = 42;
          ^ [3]


Error --------------------------------------------------------------------------------------------- excluded/test.js:8:7

Cannot assign `"hello world"` to `y` because string [1] is incompatible with number [2]. All writes to `y` must be
compatible with the type of its initializer [3]. Add an annotation to `y` [3] if a different type is desired.
[incompatible-type]

   excluded/test.js:8:7
   8|   y = "hello world";
            ^^^^^^^^^^^^^ [1]

References:
   excluded/test.js:6:9
   6| var y = 42;
              ^^ [2]
   excluded/test.js:6:5
   6| var y = 42;
          ^ [3]


Error -------------------------------------------------------------------------------------------- excluded/test.js:11:2

Cannot cast `42` to string because number [1] is incompatible with string [2]. [incompatible-cast]

   excluded/test.js:11:2
   11| (42: string); // should still have some errors!
        ^^ [1]

References:
   excluded/test.js:11:6
   11| (42: string); // should still have some errors!
            ^^^^^^ [2]


Error --------------------------------------------------------------------------------------------- included/test.js:4:5

Cannot assign `"a"` to `x` because string [1] is incompatible with number [2]. All writes to `x` must be compatible with
the type of its initializer [3]. Add an annotation to `x` [3] if a different type is desired. [incompatible-type]

   included/test.js:4:5
   4| x = "a";
          ^^^ [1]

References:
   included/test.js:3:9
   3| var x = 42;
              ^^ [2]
   included/test.js:3:5
   3| var x = 42;
          ^ [3]


Error -------------------------------------------------------------------------------------------- included/test.js:15:5

Cannot assign `g()` to `y` because string [1] is incompatible with number [2]. All writes to `y` must be compatible with
the type of its initializer [3]. Add an annotation to `y` [3] if a different type is desired. [incompatible-type]

   included/test.js:15:5
   15| y = g();
           ^^^

References:
   included/test.js:11:10
   11|   return "a";
                ^^^ [1]
   included/test.js:7:10
    7|   return 42;
                ^^ [2]
   included/test.js:14:5
   14| var y = f();
           ^ [3]


Error -------------------------------------------------------------------------------------------- included/test.js:19:5

Cannot assign `"a"` to `w` because string [1] is incompatible with number [2]. All writes to `w` must be compatible with
the type of one of its initial assignments [3], [4]. Add an annotation to `w` [3] if a different type is desired.
[incompatible-type]

   included/test.js:19:5
   19| w = "a";
           ^^^ [1]

References:
   included/test.js:18:5
   18| w = 42;
           ^^ [2]
   included/test.js:17:5
   17| var w = null;
           ^ [3]
   included/test.js:18:1
   18| w = 42;
       ^ [4]


Error -------------------------------------------------------------------------------------------- included/test.js:23:5

Cannot assign `"a"` to `z` because string [1] is incompatible with number [2]. All writes to `z` must be compatible with
the type of its initial assignment [3]. Add an annotation to `z` [4] if a different type is desired. [incompatible-type]

   included/test.js:23:5
   23| z = "a";
           ^^^ [1]

References:
   included/test.js:22:5
   22| z = 42;
           ^^ [2]
   included/test.js:22:1
   22| z = 42;
       ^ [3]
   included/test.js:21:5
   21| var z;
           ^ [4]



Found 7 errors

Only showing the most relevant union/intersection branches.
To see all branches, re-run Flow with --show-all-branches
