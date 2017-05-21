/* @flow */

import type {
  inlinedType1,
  standaloneType1,
  talias1,
  talias3,
} from "./types_only";

var a: inlinedType1 = 42;
var b: inlinedType1 = 'asdf'; // Error: string ~> number

var c: standaloneType1 = 42;
var d: standaloneType1 = 'asdf'; // Error: string ~> number

var e: talias1 = 42;
var f: talias1 = 'asdf'; // Error: string ~> number

var g: talias3 = 42;
var h: talias3 = 'asdf'; // Error: string ~> number

import type {talias4} from "./cjs_with_types";
var i: talias4 = 42;
var j: talias4 = 'asdf'; // Error: string ~> number

import {IFoo, IFoo2, IFoo3} from "./types_only";

var k: IFoo = {prop: 42};
var l: IFoo = {prop: 'asdf'}; // Error: {prop:string} ~> {prop:number}

var m: IFoo2 = {prop: 'asdf'};
var n: IFoo2 = {prop: 42}; // Error: {prop:number} ~> {prop:string}

class Foo31 {
  static prop: string;
}
var o: IFoo3 = new Foo31;
var P: Class<IFoo3> = Foo31;

class Foo32 {
  static prop: number;
}
var q: IFoo3 = new Foo32; // Error {static prop: number} ~> {static prop: string}
var R: Class<IFoo3> = Foo32; // Error {static prop: number} ~> {static prop: string}
