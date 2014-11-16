/**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 */

test1 = function () {
    var file1 = '<?hh class A { public function foo(): void { }}';
    var file2 = '<?hh class B extends A { public function bar(): void { $this->AUTO332 } }';
    hh_add_file('file1', file1);
    hh_add_file('file2', file2);
    var output1 = ''+hh_auto_complete('file2');
    var expected1 = '[0,[0,"foo (function(): void)","File \\"file1\\", line 1, characters 32-34:"],[0,"bar (function(): void)","File \\"file2\\", line 1, characters 42-44:"]]';
    file2 = '<?hh class B { public function bar(): void { $this->AUTO332 } }';
    hh_add_file('file2', file2);
    var output2 = hh_auto_complete('file2');
    var expected2 = '[0,[0,"bar (function(): void)","File \\"file2\\", line 1, characters 32-34:"]]';
    if(output1 == expected1 && output2 == expected2) {
        console.log('Test1:   OK');
    }
    else {
        console.log('Test1:   ERROR');
    }
};

// strict-mode
test2 = function () {
    var file = '<?hh // strict \n\
class Test2 { public function bar(): void { $this->AUTO332 } }';
    hh_add_file('file', file);
    var output = hh_auto_complete('file');
    var expected = '[0,[0,"bar (function(): void)","File \\"file\\", line 2, characters 31-33:"]]';
    if(output == expected) {
        console.log('Test2:   OK');
    }
    else {
        console.log('Test2:   ERROR');
    }
}


// self
test3 = function () {
    var file = '<?hh \n\
class Test3 { public static function bar(): void { self::AUTO332 } }';
    hh_add_file('file', file);
    var output = hh_auto_complete('file');
    var expected = '[0,[0,"bar (function(): void)","File \\"file\\", line 2, characters 38-40:"]]';
    if(output == expected) {
        console.log('Test3:   OK');
    }
    else {
        console.log('Test3:   ERROR');
    }
}

// one line comment
test4 = function () {
    var file = '<?hh\n\
    class WithComment {\n\
    // Comment breaks autocomplete\n\
                       public function testd(int $x, int $y): void {$this->AUTO332 }\n\
                       public function food(): void { }\n\
                       public int $x;\n\
                      }\n';
    hh_add_file('file', file);
    var output = hh_auto_complete('file');
    var expected = '[0,[0,"x int","File \\"file\\", line 6, characters 31-33:"],[0,"testd (function(int $x, int $y): void)","File \\"file\\", line 4, characters 40-44:"],[0,"food (function(): void)","File \\"file\\", line 5, characters 40-43:"]]';
    if(output == expected) {
        console.log('Test4:   OK');
    }
    else {
        console.log('Test4:   ERROR');
    }
}

// ViewerContext
test5 = function () {
    var file = '<?hh\n\
class ViewerContext {\n\
  public static function newUnpreparedAllPowerfulViewerContext(\n\
      $account_id,\n\
      $user_id,\n\
      $app_id=0): ViewerContext {\n\
    $viewer_context = static::newUnpreparedViewerContext(\n\
      $account_id,\n\
      $user_id,\n\
      $app_id\n\
    );\n\
    self::AUTO332\n\
  }\n\
\n\
}\n';
    hh_add_file('file', file);
    var output = hh_auto_complete('file');
    var expected = '[0,[0,"newUnpreparedAllPowerfulViewerContext (function($account_id, $user_id, $app_id): ViewerContext)","File \\"file\\", line 3, characters 26-62:"]]';
    if(output == expected) {
        console.log('Test5:   OK');
    }
    else {
        console.log('Test5:   ERROR');
    }
}


// Syntax error
test6 = function() {
    var file = '<?hh [;';
    hh_add_file('file', file);
    try {
        hh_auto_complete('file');
        console.log('Test6:   OK');
    }
    catch(e) {
        console.log('Test6:   ERROR');
    }
}

test7 = function() {
    hh_add_file('file3', '<?hh// strict \n function foo(ZZ $x): bool { return $x->foo(); }');
    var output = hh_check_file('file3');
    var expected = '"No errors\\n"';
    if(output == expected) {
        console.log('Test7:   OK');
    }
    else {
        console.log('Test7:   ERROR');
    }
}

// Checking that PHP files are ignored by the checker
test8 = function() {
    hh_add_file('file4', '<?php function foo() { }');
    // Calling a PHP function with too many arguments
    hh_add_file('file5', '<?hh function test() { foo(1, 2); }');
    var output = hh_check_file('file5');
    var expected = '"No errors\\n"';
    if(output == expected) {
        console.log('Test8:   OK');
    }
    else {
        console.log('Test8:   ERROR');
    }
}


// Checking that if an error in a php file is ignored, the next one
// is still captured
test9 = function() {
    hh_add_file('file6', '<?php function foo() { }');
    // Calling a PHP function with too many arguments
    hh_add_file('file7', '<?hh function test(): void { foo(1, 2); return false; }');
    var output = hh_check_file('file7');
    var expected = '"File \\"file7\\", line 1, characters 48-52:\\nInvalid return type\\nFile \\"file7\\", line 1, characters 23-26:\\nThis is void\\nFile \\"file7\\", line 1, characters 48-52:\\nIt is incompatible with a bool\\n"';
    if(output == expected) {
        console.log('Test9:   OK');
    }
    else {
        console.log('Test9:   ERROR');
    }
}

// Typing-at-pos test

test10 = function() {
    hh_add_file('file10', '<?hh function foo() { $xxxx = 0; }');
    output = hh_infer_type('file10', 1, 25);
    if(output === '"int"') {
        console.log('Test10:  OK');
    }
    else {
        console.log('Test10:  ERROR');
    }
}


// Auto-complete in if
test11 = function () {
    hh_add_file('file11', '<?php class X { public function foo() { if(true) { $this->AUTO332 }}}');
    var output = hh_auto_complete('file11');
    var expected = '[0,[0,"foo (function(): _)","File \\"file11\\", line 1, characters 33-35:"]]';
    if(output === expected) {
        console.log('Test11:  OK');
    }
    else {
        console.log('Test11:  ERROR');
    }
}

// Jump to definition
test12 = function () {
    hh_add_file('file12', '<?php class XYZ { public function foo() {}}');
    hh_add_file('file13', '<?hh function test(XYZ $x) { $x->foo();}');
    var output = hh_infer_pos('file13', 1, 33);
    var expected = '"File \\"file12\\", line 1, characters 35-37:"';
    if(output === expected) {
        console.log('Test12:  OK');
    }
    else {
        console.log('Test12:  ERROR');
    }
}

// File summary
test13 = function () {
    hh_add_file('file14', '<?php class ABC { public function foo() {} public static function bar(){} } function test(){}');
    var output = hh_file_summary('file14');
    var expected = '[0,[0,"File \\"file14\\", line 1, characters 86-89:","test","function"],[0,"File \\"file14\\", line 1, characters 13-15:","ABC","class"],[0,"File \\"file14\\", line 1, characters 35-37:","ABC::foo","method"],[0,"File \\"file14\\", line 1, characters 67-69:","ABC::bar","static method"]]'
    if(output === expected) {
        console.log('Test13:  OK');
    }
    else {
        console.log(output);
        console.log(expected);
        console.log('Test13:  ERROR');
    }
}

test1();
test2();
test3();
test4();
test5();
test6();
test7();
test8();
test9();
test10();
test11();
test12();
test13();

