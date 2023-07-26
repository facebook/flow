//@flow

function test1(x: React$AbstractComponent<any>) { // Ok, we support default targs even on the builtin
  return x;
}

function test2(x: React$AbstractComponent<any,any>) { // Ok
  return x;
}

function test3(x: React$AbstractComponent<any,any,any>) { // Too many targs
  return x;
}

import type {Component} from './signature_arity';
declare const C: Component;
(C: empty); // ERROR, Type sig correctly handles defaults
(C: React$AbstractComponent<empty, number>); // ERROR

function defaultsErrorMessages(x: React$AbstractComponent<empty>): React$AbstractComponent<empty, number> {
  return x; // ERROR
}
