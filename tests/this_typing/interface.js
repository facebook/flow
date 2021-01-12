// @flow

interface A {
  prop : number,

  method(this: {prop: number}): string;

  propUnboundNonMethod: (this: {prop: number}) => string;
}

declare var AImpl: A;

// All ok - {prop: 123} is a subtype of {prop: number}
let _1 = {prop : 123, method : AImpl.method }.method();

let _2 = {prop : true, method : AImpl.method }.method(); // Error - bool ~> number

let _3 = {method : AImpl.method }.method(); // Error prop missing

// All ok - {prop: 123} is a subtype of {prop: number}
let _4 = {prop : 123, method : AImpl.propUnboundNonMethod }.method();

let _5 = {prop : true, method : AImpl.propUnboundNonMethod }.method(); // Error - bool ~> number

let _6 = {method : AImpl.propUnboundNonMethod }.method(); // Error prop missing

export default {
  AImpl
};
