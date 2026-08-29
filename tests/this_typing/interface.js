interface A {
  prop : number,

  method(this: interface {prop: number}): string;

  propUnboundNonMethod: (this: interface {prop: number}) => string;
}

export type InlineI = interface {m(): void};

declare const AImpl: A;

// All ok - {prop: 123} is a subtype of {prop: number}
let _1 = {prop : 123, method : AImpl.method }.method();

let _2 = {prop : true, method : AImpl.method }.method(); // error: boolean is incompatible with number

let _3 = {method : AImpl.method }.method(); // error: prop is missing

// All ok - {prop: 123} is a subtype of {prop: number}
let _4 = {prop : 123, method : AImpl.propUnboundNonMethod }.method();

let _5 = {prop : true, method : AImpl.propUnboundNonMethod }.method(); // Error - boolean ~> number

let _6 = {method : AImpl.propUnboundNonMethod }.method(); // Error prop missing

export default {
  AImpl
};
