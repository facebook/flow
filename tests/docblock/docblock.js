/**
 * @typechecks
 */

function foo(/*string*/ text, /*number*/ count) /*string*/ {}

 /**
  * @param {string} text
  * @param {number} count
  * @return {string}
  */
function bar(text, count) {}

function qux(/*array<array<string>>*/ x) { }

function xyzzy() /*string|array<number>*/ {}

class C {
    foo(/*string*/ text, /*number*/ count) /*string*/ {}

    /**
     * @param {string} text
     * @param {number} count
     * @return {string}
     */
    bar(text, count) {}

    waldo(/*function(string,number):number*/ y) { }

    corge(/*?number*/ z) { }
}

var f = function(/*number*/ x) /*string*/ { }
C.o = {
    f(/*number*/ x) /*string*/ { },

    /* @param string y
       @return number
    */
    g(y) { },
    h: function(/*number*/ z) /*string*/ { }
}
