import * as React from 'react';

const Hoisted = <Foo />; // OK

component Foo() { return <div /> }
Foo = 3; // ERROR, cannot reassign components
var Foo = 3; // ERROR, cannot re-bind components
let Foo = 3; // ERROR, still can't re-bind
const Foo = 3; // ERROR
component Foo() { return <div /> } // ERROR, can't re-bind a component with a component either
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
component A() { return <div /> } // ERROR, can't re-bind other bindings with a component either!

var B = 3;
component B() { return <div /> } // ERROR

let C = 3; // ERROR. Components are hoisted so the error is on the let
component C() { return <div /> }

const D = 3;
component D() { return <div /> } // ERROR

type E = number;
component E() { return <div /> } // ERROR

function paramsStrict(F: number) {
  'use strict';
  component F() { return <div /> } // ERROR
}

function params(G: number) {
  component G() { return <div /> } // ERROR
}

declare const H: number;
component H() { return <div /> } // ERROR

declare function I(): void;
component I() { return <div /> } // ERROR

declare type J = number;
component J() { return <div /> } // ERROR

declare let K: number;
component K() { return <div /> } // ERROR

declare class L {}
component L() { return <div /> } // ERROR

class M {} // ERROR
component M() { return <div /> }
