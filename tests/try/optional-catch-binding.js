/***
 * @flow
 */

// local use of annotated var within catch is ok
function f1() {
  try {
  } catch {
    var x:number = 0;
    var y:number = x;
  }
}

// but not across try/catch
function f2() {
  try {
    var x:number = 0;
  } catch {
    var y:number = x; // error
  }
}

// it type checks the block correctly and errors
function f3() {
  try {
  } catch {
    var y:number = 'string'; // error
  }
}

// scope works
function f4() {
  var x: number = 0;
  try {
  } catch {
    var y:string = x; // error
  }
}
