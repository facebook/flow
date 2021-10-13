//@flow

import * as React from 'react';

const tests = [
  function () {
    var x = 42;
    x;
    x = "hello" //rename
    x;
  },
  function () {
    var x = null;
    x;
    x = "hello" //don't rename, provider
    x;
  },
  function () {
    var x = 42;
    x;
    x = "hello" //don't rename, havoced
    x;
    function w() {
      x;
    }
  },
  function () {
    var x = 42;
    x;
    if (condition) {
      x = "hello" //don't rename, multiple paths
    }
    x;
  },
  function () {
    var x = 42;
    x;
    if (condition) {
      x = "hello" //don't rename, multiple paths, even tho next line is exclusive
      x;
    }
    x;
  },
  function () {
    var x = 42;
    x;
    if (condition) {
      x = "hello" //do rename, because no reads after and outside of the conditonal
      x;
    }
  },
  function () {
    console.log();
    x;
    var x = 42;
    x = "hello"; // don't rename, havoced above
    x;
    function f() {
      x = 42;
    }
  },
  function () {
    var x = null;
    function f() {
      x = 42;
      x = false; // rename
      x;
    }
  },
  function () {
    var x = null;
    function f() {
      x;
      x = 42;
      x = false; // don't rename, read of x at top of this scope is havoced
      x;
    }
  },
  function () {
    var x = null;
    function f() {
      x = 42;
      function w() {
        x;
      }
      x = false; // don't rename, read of x in w is havoed
      x;
    }
  },
  function () {
    var x = 42;
    var xStr$Temp1 = 42
    x;
    x = "hello" //rename, and dont collide with the in scope var
    x;
  },
  function () {
    var x = 42;
    function f() { x = null }
    x = "hello" //rename, no havocing occurred
    x;
  },
  function () {
    var x = 42;
    function f() { x = null }
    x = "hello" //don't rename, havoc
    console.log()
    x;
  },
  function () {
    declare function create(any): string;
    var x = 42;
    x = create(x); // rename to xContainer
  },
  function () {
    declare function createRadContainer(any): string;
    var x = 42;
    x = createRadContainer(x); //rename to xRad
  },
  function () {
    declare var react_: {
      memo(any): string
    }
    var x = 42;
    x = react_.memo(x); //rename to xMemo
  },
  function () {
    declare var react_: {
      createWithSweetData(any): string
    }
    var x = 42;
    x = react_.createWithSweetData(x); //rename to xSweetData
  },
  function () {
    var x = true;
    x = 314 // rename to x314
  },
  function () {
    var x = true;
    x = 3.14 // rename to x3, to avoid `.`
  },
  function () {
    var x = true;
    x = .314 //rename to xNumber; no prefix matches
  },
  function () {
    var x = true;
    x = 314453453453454554 //too long, rename to xNumber (modulo temporaries)
  },
  function () {
    declare var z: { bluh: number }
    var x = true;
    x = z.bluh; // rename to xBluh
  },
  function () {
    declare var z: number;
    var x = true;
    x = z; // rename to xZ
  },
  function () {
    declare var react_: {
      memo(any): string,
      createWithSweetData(any): string,
      createCoolContainer(any): string,
    }
    var z = true;
    z = react_.memo(z); // rename all of these
    z = react_.createWithSweetData(z);
    z = react_.createCoolContainer(z);
  },
  function () {
    var prop = 42;
    prop = "a";

    function takesPropObj(p: { prop: string }) { }

    takesPropObj( {prop} ); // rename prop, make non-shorthand { prop: propStr }
  },
  function () {
    function f() {
      return <Ct />
    }

    let Ct = ( props: { a: number }) => { return <div>Hi</div> }
    Ct = ( props: { b: string }) => { return <div>Hi</div> } // no rename
    <Ct />
  },
  function () {
    let Ct = ( props: { a: number }) => { return <div>Hi</div> }
    Ct = ( props: { b: string }) => { return <div>Hi</div> } // rename
    <Ct />
  },
  function () {
    declare var x: Array<string>;
    for (let y = "a";;) {
      y; // don't rename
      y = 42;
    }
  },
  function () {
    declare var x: Array<string>;
    for (let y of [1,2,3]) {
      y; //don't rename
      y = 42;
    }
  },
]
