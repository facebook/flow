// @flow

class A {
  prop: boolean;
  static prop : boolean;

  method(this: interface {prop: boolean}): string {
    return this.prop; // Error - number ~> string
  }

  static staticMethod(this: interface {prop: boolean}): string {
    return this.prop; // Error - number ~> string
  }

  propUnboundNonMethod : (this: interface {prop: boolean}) => string = function(this: interface {prop: boolean}): string {
    return this.prop; // Error - number ~> string
  }

  static staticPropUnboundNonMethod : (this: interface {prop: boolean}) => string = function(this: interface {prop: boolean}): string {
    return this.prop; // Error - number ~> string
  }
}

// All ok - {prop: true} is a subtype of {prop: boolean}
let _1 = {prop : true, method : (new A()).method }.method();

let _2 = {prop : 123, method : (new A()).method }.method(); // Error - bool ~> number

let _3 = {method : (new A()).method }.method(); // Error prop missing

// All ok - {prop: true} is a subtype of {prop: boolean}
let _4 = {prop : true, method : A.staticMethod }.method();

let _5 = {prop : 123, method : A.staticMethod }.method();// Error - bool ~> number

let _6 = {method : A.staticMethod }.method(); // error prop missing

// All ok - {prop: true} is a subtype of {prop: boolean}
let _7 = {prop : true, method : (new A()).propUnboundNonMethod }.method();

let _8 = {prop : 123, method : (new A()).propUnboundNonMethod }.method(); // Error - bool ~> number

let _9 = {method : (new A()).propUnboundNonMethod }.method(); // Error prop missing

// All ok - {prop: true} is a subtype of {prop: boolean}
let _10 = {prop : true, method : A.staticPropUnboundNonMethod }.method();

let _11 = {prop : 123, method : A.staticPropUnboundNonMethod }.method();// Error - bool ~> number

let _12 = {method : A.staticPropUnboundNonMethod }.method(); // error prop missing

export default {
  A
};
