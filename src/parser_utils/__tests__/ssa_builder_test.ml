(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)


open OUnit2
open Test_utils

module LocMap = Utils_js.LocMap

let mk_ssa_builder_test contents expected_values =
  begin fun ctxt ->
    let values = Ssa_builder.program (parse contents) in
    let printer = Ssa_api.print_values in
    assert_equal ~ctxt
      ~cmp:(eq printer)
      ~printer
      ~msg:"SSA values don't match!"
      expected_values values
  end

let mk_write pos1 pos2 =
  Ssa_api.Write (mk_loc pos1 pos2)

let tests = "ssa_builder" >::: [
  "var" >:: mk_ssa_builder_test
    "function foo(x) {
       var y;
       if (x) y = 123;
       return y;
     }"
    LocMap.(
      empty |>
      add (mk_loc (3, 11) (3, 12)) [ (* x *)
        mk_write (1, 13) (1, 14);
      ] |>
      add (mk_loc (4, 14) (4, 15)) [ (* y *)
        Ssa_api.uninitialized;
        mk_write (3, 14) (3, 15);
      ]
    );
  "var_hoist" >:: mk_ssa_builder_test
    "function foo(x) {
       y = x;
       var y;
       return y;
     }"
    LocMap.(
      empty |>
      add (mk_loc (2, 11) (2, 12)) [ (* x *)
        mk_write (1, 13) (1, 14);
      ] |>
      add (mk_loc (4, 14) (4, 15)) [ (* y *)
        mk_write (2, 7) (2, 8);
      ]
    );
  "let" >:: mk_ssa_builder_test
    "function foo() { \
       let x = 0; \
       return x; \
     }"
    LocMap.(
      empty |>
      add (mk_loc (1, 35) (1, 36)) [
        mk_write (1, 21) (1, 22);
      ]
    );
  "let_update" >:: mk_ssa_builder_test
    "function foo() { \
       let x = 0; \
       x++; \
       return x; \
     }"
    LocMap.(
      empty |>
      add (mk_loc (1, 28) (1, 29)) [
        mk_write (1, 21) (1, 22);
      ] |>
      add (mk_loc (1, 40) (1, 41)) [
        mk_write (1, 28) (1, 29);
      ]
    );
  "if" >:: mk_ssa_builder_test
    "(function() { \
        var xxx = 0; \
        let yyy = 1; \
        if (yyy) { \
          yyy = 2; \
        } else { } \
        xxx = yyy; \
      })"
    LocMap.(
      empty |>
      add (mk_loc (1, 44) (1, 47)) [
        mk_write (1, 31) (1, 34);
      ] |>
      add (mk_loc (1, 77) (1, 80)) [
        mk_write (1, 31) (1, 34);
        mk_write (1, 51) (1, 54);
      ]
    );
  "if_let" >:: mk_ssa_builder_test
    "(function() { \
        var xxx = 0; \
        let yyy = 1; \
        if (yyy) { \
          let yyy = 2; \
        } else { } \
        xxx = yyy; \
      })"
    LocMap.(
      empty |>
      add (mk_loc (1, 44) (1, 47)) [
        mk_write (1, 31) (1, 34);
      ] |>
      add (mk_loc (1, 81) (1, 84)) [
        mk_write (1, 31) (1, 34);
      ]
    );
  "while" >:: mk_ssa_builder_test
    "(function() { \
        var xxx = 0; \
        let yyy = 1; \
        while (yyy) { \
          yyy = 2; \
        } \
        xxx = yyy; \
      })"
    LocMap.(
      empty |>
      add (mk_loc (1, 47) (1, 50)) [
        mk_write (1, 31) (1, 34);
        mk_write (1, 54) (1, 57);
      ] |>
      add (mk_loc (1, 71) (1, 74)) [
        mk_write (1, 31) (1, 34);
        mk_write (1, 54) (1, 57);
      ]
    );
  "while_let" >:: mk_ssa_builder_test
    "(function() { \
        var xxx = 0; \
        let yyy = 1; \
        while (yyy) { \
          let yyy = 2; \
        } \
        xxx = yyy; \
      })"
    LocMap.(
      empty |>
      add (mk_loc (1, 47) (1, 50)) [
        mk_write (1, 31) (1, 34);
      ] |>
      add (mk_loc (1, 75) (1, 78)) [
        mk_write (1, 31) (1, 34);
      ]
    );
  "for" >:: mk_ssa_builder_test
    "(function() { \
        var xxx = 0; \
        let yyy = 1; \
        for (var zzz = 0; zzz < 3; zzz = zzz + 1) { \
          yyy = 2; \
        } \
        xxx = yyy; \
      })"
    LocMap.(
      empty |>
      add (mk_loc (1, 58) (1, 61)) [
        mk_write (1, 49) (1, 52);
        mk_write (1, 67) (1, 70);
      ] |>
      add (mk_loc (1, 73) (1, 76)) [
        mk_write (1, 49) (1, 52);
        mk_write (1, 67) (1, 70);
      ] |>
      add (mk_loc (1, 101) (1, 104)) [
        mk_write (1, 31) (1, 34);
        mk_write (1, 84) (1, 87);
      ]
    );
  "for_let" >:: mk_ssa_builder_test
    "(function() { \
        var xxx = 0; \
        let yyy = 1; \
        for (let zzz = 0; zzz < 3; zzz = zzz + 1) { \
          yyy = 2; \
        } \
        xxx = yyy; \
      })"
    LocMap.(
      empty |>
      add (mk_loc (1, 58) (1, 61)) [
        mk_write (1, 49) (1, 52);
        mk_write (1, 67) (1, 70);
      ] |>
      add (mk_loc (1, 73) (1, 76)) [
        mk_write (1, 49) (1, 52);
        mk_write (1, 67) (1, 70);
      ] |>
      add (mk_loc (1, 101) (1, 104)) [
        mk_write (1, 31) (1, 34);
        mk_write (1, 84) (1, 87);
      ]
    );
  "switch" >:: mk_ssa_builder_test
    "(function() { \
        var a = 0; \
        switch (a + 1) { \
          case a: \
            a = a + 1; \
          case a + 1: \
            a = a + 1; \
          default: \
            a = a + 1; \
        } \
        return a; \
      })"
    LocMap.(
      empty |>
      add (mk_loc (1, 33) (1, 34)) [
        mk_write (1, 18) (1, 19);
      ] |>
      add (mk_loc (1, 47) (1, 48)) [
        mk_write (1, 18) (1, 19);
      ] |>
      add (mk_loc (1, 54) (1, 55)) [
        mk_write (1, 18) (1, 19);
      ] |>
      add (mk_loc (1, 66) (1, 67)) [
        mk_write (1, 18) (1, 19);
      ] |>
      add (mk_loc (1, 77) (1, 78)) [
        mk_write (1, 18) (1, 19);
        mk_write (1, 50) (1, 51);
      ] |>
      add (mk_loc (1, 97) (1, 98)) [
        mk_write (1, 18) (1, 19);
        mk_write (1, 73) (1, 74);
      ] |>
      add (mk_loc (1, 113) (1, 114)) [
        mk_write (1, 18) (1, 19);
        mk_write (1, 93) (1, 94);
      ]
    );
  "try" >:: mk_ssa_builder_test
    "(function() { \
        var xxx = 0; \
        let yyy = 1; \
        try { \
          yyy = 2; \
        } catch (e) { \
          xxx = yyy; \
        } \
        return xxx; \
      })"
    LocMap.(
      empty |>
      add (mk_loc (1, 75) (1, 78)) [
        mk_write (1, 31) (1, 34);
        mk_write (1, 46) (1, 49);
      ] |>
      add (mk_loc (1, 89) (1, 92)) [
        mk_write (1, 18) (1, 21);
        mk_write (1, 69) (1, 72);
      ]
    );
  "closure" >:: mk_ssa_builder_test
    "(function() { \
        var xxx = 0; \
        let yyy = 1; \
        function foo() { \
          xxx = yyy; \
        } \
        yyy = 2; \
        foo(); \
        return xxx; \
      })"
    LocMap.(
      empty |>
      add (mk_loc (1, 63) (1, 66)) [
        Ssa_api.uninitialized;
        mk_write (1, 31) (1, 34);
        mk_write (1, 70) (1, 73)
      ] |>
      add (mk_loc (1, 79) (1, 82)) [
        mk_write (1, 49) (1, 52);
      ] |>
      add (mk_loc (1, 93) (1, 96)) [
        mk_write (1, 18) (1, 21);
        mk_write (1, 57) (1, 60);
      ]
    );
  "break_while" >:: mk_ssa_builder_test
    "(function() { \
        var x = 0; \
        while (x) { \
          x = 1; \
          break; \
          x; \
          x = 2; \
        } \
        return x; \
      })"
    LocMap.(
      empty |>
      add (mk_loc (1, 32) (1, 33)) [
        mk_write (1, 18) (1, 19);
      ] |>
      add (mk_loc (1, 70) (1, 71)) [
        mk_write (1, 18) (1, 19);
        mk_write (1, 37) (1, 38);
      ]
    );
  "continue_while" >:: mk_ssa_builder_test
    "(function() { \
        var x = 0; \
        while (x) { \
          x = 1; \
          continue; \
          x; \
          x = 2; \
        } \
        return x; \
      })"
    LocMap.(
      empty |>
      add (mk_loc (1, 32) (1, 33)) [
        mk_write (1, 18) (1, 19);
        mk_write (1, 37) (1, 38);
      ] |>
      add (mk_loc (1, 73) (1, 74)) [
        mk_write (1, 18) (1, 19);
        mk_write (1, 37) (1, 38);
      ]
    );
  "break_for" >:: mk_ssa_builder_test
    "(function() { \
        for (var x = 0; x < 10; x++) { \
          x = 1; \
          break; \
          x; \
          x = 2; \
        } \
        return x; \
      })"
    LocMap.(
      empty |>
      add (mk_loc (1, 30) (1, 31)) [
        mk_write (1, 23) (1, 24);
      ] |>
      add (mk_loc (1, 78) (1, 79)) [
        mk_write (1, 23) (1, 24);
        mk_write (1, 45) (1, 46);
      ]
    );
  "continue_for" >:: mk_ssa_builder_test
    "(function() { \
        for (var x = 0; x < 10; x++) { \
          x = 1; \
          continue; \
          x; \
          x = 2; \
        } \
        return x; \
      })"
    LocMap.(
      empty |>
      add (mk_loc (1, 30) (1, 31)) [
        mk_write (1, 23) (1, 24);
        mk_write (1, 38) (1, 39);
      ] |>
      add (mk_loc (1, 38) (1, 39)) [
        mk_write (1, 45) (1, 46);
      ] |>
      add (mk_loc (1, 81) (1, 82)) [
        mk_write (1, 23) (1, 24);
        mk_write (1, 38) (1, 39);
      ]
    );
  "break_switch" >:: mk_ssa_builder_test
    "(function() { \
        var a = 0; \
        switch (a + 1) { \
          case a: \
            a = a + 1; \
            break; \
          case a + 1: \
            a = a + 1; \
          default: \
            a = a + 1; \
        } \
        return a; \
      })"
    LocMap.(
      empty |>
      add (mk_loc (1, 33) (1, 34)) [
        mk_write (1, 18) (1, 19);
      ] |>
      add (mk_loc (1, 47) (1, 48)) [
        mk_write (1, 18) (1, 19);
      ] |>
      add (mk_loc (1, 54) (1, 55)) [
        mk_write (1, 18) (1, 19);
      ] |>
      add (mk_loc (1, 73) (1, 74)) [
        mk_write (1, 18) (1, 19);
      ] |>
      add (mk_loc (1, 84) (1, 85)) [
        mk_write (1, 18) (1, 19);
      ] |>
      add (mk_loc (1, 104) (1, 105)) [
        mk_write (1, 18) (1, 19);
        mk_write (1, 80) (1, 81);
      ] |>
      add (mk_loc (1, 120) (1, 121)) [
        mk_write (1, 18) (1, 19);
        mk_write (1, 50) (1, 51);
        mk_write (1, 100) (1, 101);
      ]
    );
  "break_labeled" >:: mk_ssa_builder_test
    "(function() { \
        var a = 0; \
        L: { \
          a = a + 1; \
          break L; \
          a = a + 1; \
        } \
        return a; \
      })"
    LocMap.(
      empty |>
      add (mk_loc (1, 34) (1, 35)) [
        mk_write (1, 18) (1, 19);
      ] |>
      add (mk_loc (1, 70) (1, 71)) [
        mk_write (1, 30) (1, 31);
      ]
    );
  "break_labeled_while" >:: mk_ssa_builder_test
    "(function() { \
        var x = 0; \
        L: while (x) { \
          x = 1; \
          break L; \
          x; \
          x = 2; \
        } \
        return x; \
      })"
    LocMap.(
      empty |>
      add (mk_loc (1, 35) (1, 36)) [
        mk_write (1, 18) (1, 19);
      ] |>
      add (mk_loc (1, 75) (1, 76)) [
        mk_write (1, 18) (1, 19);
        mk_write (1, 40) (1, 41);
      ]
    );
  "break_if" >:: mk_ssa_builder_test
    "(function() { \
        var a = 0; \
        L: { \
          a = a + 1; \
          if (a) break L; \
          else break L; \
          a = a + 1; \
        } \
        return a; \
      })"
    LocMap.(
      empty |>
      add (mk_loc (1, 34) (1, 35)) [
        mk_write (1, 18) (1, 19);
      ] |>
      add (mk_loc (1, 45) (1, 46)) [
        mk_write (1, 30) (1, 31);
      ] |>
      add (mk_loc (1, 91) (1, 92)) [
        mk_write (1, 30) (1, 31);
      ]
    );
  "break_if_partial" >:: mk_ssa_builder_test
    "(function() { \
        var a = 0; \
        L: { \
          a = a + 1; \
          if (a) break L; \
          a = a + 1; \
        } \
        return a; \
      })"
    LocMap.(
      empty |>
      add (mk_loc (1, 34) (1, 35)) [
        mk_write (1, 18) (1, 19);
      ] |>
      add (mk_loc (1, 45) (1, 46)) [
        mk_write (1, 30) (1, 31);
      ] |>
      add (mk_loc (1, 61) (1, 62)) [
        mk_write (1, 30) (1, 31);
      ] |>
      add (mk_loc (1, 77) (1, 78)) [
        mk_write (1, 30) (1, 31);
        mk_write (1, 57) (1, 58);
      ]
    );
  "continue_if_partial" >:: mk_ssa_builder_test
    "(function() { \
        var a = 0; \
        while (a) { \
          a = a + 1; \
          if (a) continue; \
          a = a + 1; \
        } \
        return a; \
      })"
    LocMap.(
      empty |>
      add (mk_loc (1, 32) (1, 33)) [
        mk_write (1, 18) (1, 19);
        mk_write (1, 37) (1, 38);
        mk_write (1, 65) (1, 66);
      ] |>
      add (mk_loc (1, 41) (1, 42)) [
        mk_write (1, 18) (1, 19);
        mk_write (1, 37) (1, 38);
        mk_write (1, 65) (1, 66);
      ] |>
      add (mk_loc (1, 52) (1, 53)) [
        mk_write (1, 37) (1, 38);
      ] |>
      add (mk_loc (1, 69) (1, 70)) [
        mk_write (1, 37) (1, 38);
      ] |>
      add (mk_loc (1, 85) (1, 86)) [
        mk_write (1, 18) (1, 19);
        mk_write (1, 37) (1, 38);
        mk_write (1, 65) (1, 66);
      ]
    );
  "continue_labeled_while" >:: mk_ssa_builder_test
    "(function() { \
        var x = 0; \
        L: while (x) { \
          x = 1; \
          continue L; \
          x; \
          x = 2; \
        } \
        return x; \
      })"
    LocMap.(
      empty |>
      add (mk_loc (1, 35) (1, 36)) [
        mk_write (1, 18) (1, 19);
        mk_write (1, 40) (1, 41);
      ] |>
      add (mk_loc (1, 78) (1, 79)) [
        mk_write (1, 18) (1, 19);
        mk_write (1, 40) (1, 41);
      ]
    );
  "continue_labeled_do_while" >:: mk_ssa_builder_test
    "(function() { \
        var x = 0; \
        L: do { \
          x = 1; \
          continue L; \
          x; \
          x = 2; \
        } while (x) \
        return x; \
      })"
    LocMap.(
      empty |>
      add (mk_loc (1, 71) (1, 72)) [
        mk_write (1, 33) (1, 34);
      ] |>
      add (mk_loc (1, 81) (1, 82)) [
        mk_write (1, 33) (1, 34);
      ]
    );
  "labeled_break_do_while" >:: mk_ssa_builder_test
    "(function() { \
        var x = 0; \
        L: { \
          do { \
            x = 1; \
            break L; \
            x = 2; \
          } while (true); \
          x = 3; \
        } \
        return x; \
      })"
    LocMap.(
      empty |>
      add (mk_loc (1, 90) (1, 91)) [
        mk_write (1, 35) (1, 36);
      ]
    );
  "labeled_break_try_catch" >:: mk_ssa_builder_test
    "(function() { \
        var x = 0; \
        L: \
          try { \
            x = x + 1; \
          } catch (e) { \
            x = e + 1; \
            break L; \
          } finally { \
            x = x + 1; \
          } \
        return x; \
      })"
    LocMap.(
      empty |>
      add (mk_loc (1, 38) (1, 39)) [
        mk_write (1, 18) (1, 19);
      ] |>
      add (mk_loc (1, 63) (1, 64)) [
        mk_write (1, 54) (1, 55);
      ] |>
      add (mk_loc (1, 95) (1, 96)) [
        mk_write (1, 18) (1, 19);
        mk_write (1, 34) (1, 35);
        mk_write (1, 59) (1, 60);
        mk_write (1, 91) (1, 92);
      ] |>
      add (mk_loc (1, 111) (1, 112)) [
        mk_write (1, 91) (1, 92);
      ]
    );
  "nested_labeled_break_try_catch" >:: mk_ssa_builder_test
    "(function() { \
        var x = 0; \
        M: { \
          L: { \
            try { \
              x = x + 1; \
            } catch (e) { \
              x = e + 1; \
              break L; \
            } finally { \
              x = x + 1; \
              break M; \
            } \
          } \
          x = x + 1; \
        } \
        return x; \
      })"
    LocMap.(
      empty |>
      add (mk_loc (1, 45) (1, 46)) [
        mk_write (1, 18) (1, 19);
      ] |>
      add (mk_loc (1, 70) (1, 71)) [
        mk_write (1, 61) (1, 62);
      ] |>
      add (mk_loc (1, 102) (1, 103)) [
        mk_write (1, 18) (1, 19);
        mk_write (1, 41) (1, 42);
        mk_write (1, 66) (1, 67);
        mk_write (1, 98) (1, 99);
      ] |>
      add (mk_loc (1, 142) (1, 143)) [
        mk_write (1, 98) (1, 99);
      ]
    );
  "throw" >:: mk_ssa_builder_test
    "(function() { \
        var x = 0; \
        if (x) { \
          x = 1; \
          throw x; \
          x = 2; \
        } \
        return x; \
      })"
    LocMap.(
      empty |>
      add (mk_loc (1, 29) (1, 30)) [
        mk_write (1, 18) (1, 19);
      ] |>
      add (mk_loc (1, 47) (1, 48)) [
        mk_write (1, 34) (1, 35);
      ] |>
      add (mk_loc (1, 66) (1, 67)) [
        mk_write (1, 18) (1, 19);
      ]
    );
  "nested_while" >:: mk_ssa_builder_test
    "(function() { \
        var x = 0; \
        while (x) { \
          x = 1; \
          if (x) { break; } \
          x = 2; \
          while (x) { \
            break; \
          } \
          x = x + 1 \
        } \
        return x; \
      })"
    LocMap.(
      empty |>
      add (mk_loc (1, 32) (1, 33)) [
        mk_write (1, 18) (1, 19);
        mk_write (1, 90) (1, 91);
      ] |>
      add (mk_loc (1, 48) (1, 49)) [
        mk_write (1, 37) (1, 38);
      ] |>
      add (mk_loc (1, 76) (1, 77)) [
        mk_write (1, 62) (1, 63);
      ] |>
      add (mk_loc (1, 94) (1, 95)) [
        mk_write (1, 62) (1, 63);
      ] |>
      add (mk_loc (1, 109) (1, 110)) [
        mk_write (1, 18) (1, 19);
        mk_write (1, 37) (1, 38);
        mk_write (1, 90) (1, 91);
      ]
    );
  "JSX" >:: mk_ssa_builder_test
    "class Foo {}; <Foo></Foo>; <Foo/>"
    LocMap.(
      empty |>
      add (mk_loc (1, 15) (1, 18)) [
        mk_write (1, 6) (1, 9);
      ] |>
      add (mk_loc (1, 21) (1, 24)) [
        mk_write (1, 6) (1, 9);
      ] |>
      add (mk_loc (1, 28) (1, 31)) [
        mk_write (1, 6) (1, 9);
      ]
    );
]
