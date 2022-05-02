Error ------------------------------------------------------------------------------------------------- Closure.js:20:14

Cannot call `takes_string` with `global_x` bound to `_` because number [1] is incompatible with string [2].
[incompatible-call]

   Closure.js:20:14
   20| takes_string(global_x); // error (can't distinguish between calls to global_f and global_g)
                    ^^^^^^^^

References:
   Closure.js:12:15
   12| var global_x: number | string = 'hello';
                     ^^^^^^ [1]
   Closure.js:7:26
    7| function takes_string(_: string) {}
                                ^^^^^^ [2]


Error ------------------------------------------------------------------------------------------------- Closure.js:23:14

Cannot call `takes_string` with `global_x` bound to `_` because number [1] is incompatible with string [2].
[incompatible-call]

   Closure.js:23:14
   23| takes_string(global_x);
                    ^^^^^^^^

References:
   Closure.js:12:15
   12| var global_x: number | string = 'hello';
                     ^^^^^^ [1]
   Closure.js:7:26
    7| function takes_string(_: string) {}
                                ^^^^^^ [2]


Error ------------------------------------------------------------------------------------------------- Closure.js:39:16

Cannot call `takes_string` with `local_x` bound to `_` because number [1] is incompatible with string [2].
[incompatible-call]

   Closure.js:39:16
   39|   takes_string(local_x); // error (can't distinguish between calls to local_f and local_g)
                      ^^^^^^^

References:
   Closure.js:31:25
   31|   var local_x: string | number = 'hello';
                               ^^^^^^ [1]
   Closure.js:7:26
    7| function takes_string(_: string) {}
                                ^^^^^^ [2]


Error ------------------------------------------------------------------------------------------------- Closure.js:42:16

Cannot call `takes_string` with `local_x` bound to `_` because number [1] is incompatible with string [2].
[incompatible-call]

   Closure.js:42:16
   42|   takes_string(local_x); // error
                      ^^^^^^^

References:
   Closure.js:31:25
   31|   var local_x: string | number = 'hello';
                               ^^^^^^ [1]
   Closure.js:7:26
    7| function takes_string(_: string) {}
                                ^^^^^^ [2]


Error ------------------------------------------------------------------------------------------------- Closure.js:60:14

Cannot call `takes_string` with `global_y` bound to `_` because number [1] is incompatible with string [2].
[incompatible-call]

   Closure.js:60:14
   60| takes_string(global_y); // error (can't distinguish between calls to global_o.f and global_o.g)
                    ^^^^^^^^

References:
   Closure.js:50:24
   50| var global_y: string | number = 'hello';
                              ^^^^^^ [1]
   Closure.js:7:26
    7| function takes_string(_: string) {}
                                ^^^^^^ [2]


Error ------------------------------------------------------------------------------------------------- Closure.js:63:14

Cannot call `takes_string` with `global_y` bound to `_` because number [1] is incompatible with string [2].
[incompatible-call]

   Closure.js:63:14
   63| takes_string(global_y); // error
                    ^^^^^^^^

References:
   Closure.js:50:24
   50| var global_y: string | number = 'hello';
                              ^^^^^^ [1]
   Closure.js:7:26
    7| function takes_string(_: string) {}
                                ^^^^^^ [2]


Error ------------------------------------------------------------------------------------------------- Closure.js:81:16

Cannot call `takes_string` with `local_y` bound to `_` because number [1] is incompatible with string [2].
[incompatible-call]

   Closure.js:81:16
   81|   takes_string(local_y); // error (can't distinguish between calls to local_o.f and local_o.g)
                      ^^^^^^^

References:
   Closure.js:71:25
   71|   var local_y: string | number = 'hello';
                               ^^^^^^ [1]
   Closure.js:7:26
    7| function takes_string(_: string) {}
                                ^^^^^^ [2]


Error ------------------------------------------------------------------------------------------------- Closure.js:84:16

Cannot call `takes_string` with `local_y` bound to `_` because number [1] is incompatible with string [2].
[incompatible-call]

   Closure.js:84:16
   84|   takes_string(local_y); // error
                      ^^^^^^^

References:
   Closure.js:71:25
   71|   var local_y: string | number = 'hello';
                               ^^^^^^ [1]
   Closure.js:7:26
    7| function takes_string(_: string) {}
                                ^^^^^^ [2]


Error -------------------------------------------------------------------------------------------------- Closure.js:95:4

Cannot cast `x` to string because number [1] is incompatible with string [2]. [incompatible-cast]

   Closure.js:95:4
   95|   (x: string); // blame
          ^

References:
   Closure.js:93:10
   93|   var x: number | string = 'hello world';
                ^^^^^^ [1]
   Closure.js:95:7
   95|   (x: string); // blame
             ^^^^^^ [2]


Error ------------------------------------------------------------------------------------------------- Closure.js:105:6

Cannot cast `x` to string because number [1] is incompatible with string [2]. [incompatible-cast]

   Closure.js:105:6
   105|     (x: string); // blame
             ^

References:
   Closure.js:102:10
   102|   var x: number | string = 'hello world';
                 ^^^^^^ [1]
   Closure.js:105:9
   105|     (x: string); // blame
                ^^^^^^ [2]


Error ------------------------------------------------------------------------------------------------- Closure.js:132:4

Cannot cast `x` to undefined because number [1] is incompatible with undefined [2]. [incompatible-cast]

   Closure.js:132:4
   132|   (x: void); // should error
           ^

References:
   Closure.js:126:10
   126|   var x: number;
                 ^^^^^^ [1]
   Closure.js:132:7
   132|   (x: void); // should error
              ^^^^ [2]


Error ------------------------------------------------------------------------------------------------- Closure.js:136:3

Cannot resolve name `x`. [cannot-resolve-name]

   136|   x;
          ^


Error ------------------------------------------------------------------------------------------------- Closure.js:141:4

Cannot cast `x` to undefined because number [1] is incompatible with undefined [2]. [incompatible-cast]

   Closure.js:141:4
   141|   (x: void); // should error
           ^

References:
   Closure.js:138:9
   138|     x = 42;
                ^^ [1]
   Closure.js:141:7
   141|   (x: void); // should error
              ^^^^ [2]


Error ---------------------------------------------------------------------------------------------- cond_havoc.js:12:10

Cannot return `x` because string [1] is incompatible with number [2]. [incompatible-return]

   cond_havoc.js:12:10
   12|   return x; // error, string ~/~> number (return type anno) TODO
                ^

References:
   cond_havoc.js:7:19
    7|   var x: number | string = 0;
                         ^^^^^^ [1]
   cond_havoc.js:6:28
    6| function example(b: bool): number {
                                  ^^^^^^ [2]


Error --------------------------------------------------------------------------------------------------- const.js:20:38

Cannot assign `var_x` to `y` because null or undefined [1] is incompatible with number [2]. [incompatible-type]

   const.js:20:38
   20|     call_me = () => { var y:number = var_x; };  // error
                                            ^^^^^

References:
   const.js:17:14
   17|   var var_x: ?number = x;
                    ^^^^^^^ [1]
   const.js:20:29
   20|     call_me = () => { var y:number = var_x; };  // error
                                   ^^^^^^ [2]



Found 15 errors
