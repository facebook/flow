// @flow

declare class A {
  prop : number,
  static prop : number,

  method(this: interface {prop: number}): string;

  static staticMethod(this: interface {prop: number}): string;

  propUnboundNonMethod: (this: interface {prop: number}) => string;

  static staticPropUnboundNonMethod: (this: interface {prop: number}) => string;
}

// All ok - {prop: 123} is a subtype of {prop: number}
let _1 = {prop : 123, method : (new A()).method }.method();

let _2 = {prop : true, method : (new A()).method }.method(); // Error - bool ~> number

let _3 = {method : (new A()).method }.method(); // Error prop missing

// All ok - {prop: 123} is a subtype of {prop: number}
let _4 = {prop : 123, method : A.staticMethod }.method();

let _5 = {prop : true, method : A.staticMethod }.method();// Error - bool ~> number

let _6 = {method : A.staticMethod }.method(); // error prop missing

// All ok - {prop: 123} is a subtype of {prop: number}
let _7 = {prop : 123, method : (new A()).propUnboundNonMethod }.method();

let _8 = {prop : true, method : (new A()).propUnboundNonMethod }.method(); // Error - bool ~> number

let _9 = {method : (new A()).propUnboundNonMethod }.method(); // Error prop missing

// All ok - {prop: 123} is a subtype of {prop: number}
let _10 = {prop : 123, method : A.staticPropUnboundNonMethod }.method();

let _11 = {prop : true, method : A.staticPropUnboundNonMethod }.method();// Error - bool ~> number

let _12 = {method : A.staticPropUnboundNonMethod }.method(); // error prop missing

export default {
  A
};
