import * as React from 'react';

const Hoisted = <Foo />; // OK

component Foo() {}
Foo = 3; // ERROR, cannot reassign components
var Foo = 3; // ERROR, cannot re-bind components
let Foo = 3; // ERROR, still can't re-bind
const Foo = 3; // ERROR
component Foo() {} // ERROR, can't re-bind a component with a component either
type Foo = number; // ERROR
function Foo() {} // ERROR
class Foo {} // ERROR
declare var Foo: number; // ERROR
declare let Foo: number; // ERROR
declare const Foo: number; // ERROR
declare type Foo = number; // ERROR
declare function Foo(): number; // ERROR
declare class Foo {} // ERROR
function Bar(Foo: number) {} // OK

function A() {}
component A() {} // ERROR, can't re-bind other bindings with a component either!

var B = 3;
component B() {} // ERROR

let C = 3; // ERROR. Components are hoisted so the error is on the let
component C() {}

const D = 3;
component D() {} // ERROR

type E = number;
component E() {} // ERROR

function paramsStrict(F: number) {
  'use strict';
  component F() {} // ERROR
}

function params(G: number) {
  component G() {} // ERROR
}

declare const H: number;
component H() {} // ERROR

declare function I(): void;
component I() {} // ERROR

declare type J = number;
component J() {} // ERROR

declare let K: number;
component K() {} // ERROR

declare class L {}
component L() {} // ERROR

class M {} // ERROR
component M() {}
