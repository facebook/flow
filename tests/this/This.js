/*
 * this bindings:
 */

/* standard functions may rebind this */
function f1(this: typeof Object.prototype | {|x: number|} | {|x: string|}) : number {
  return this.x // error: x not in Object, string not compatible with number
}

var f1_1 = f1.bind({x: 0})();            // ok
var f1_2 : string = f1.bind({x: 0})();   // error, number -> string
var f1_3 = f1.bind({x: ""})();           // error, string -> number
// TODO make this error blame the call site, rather than the function body
var f1_4 = f1();                         // error, (global object).x

/* arrow functions bind this at point of definition */
/* top level arrow functions bind this to global object */
var a1 = () => {
  return this.x
}

var ax = a1();

/* nested arrows bind enclosing this (which may itself rebind) */
function f2(this: typeof Object.prototype | {|x: number|} | {|x: string|}) : number {
    var a2 = () => { return this.x }; // error: x not in Object
    return a2() // error: string not compatible with number
}

var f2_1 = f2.bind({x: 0})();            // ok
var f2_2 : string = f2.bind({x: 0})();   // error, number -> string
var f2_3 = f2.bind({x: ""})();
// TODO make this error blame the call site, rather than the function body
var f2_4 = f2();

(this: void);

module.exports = true;
