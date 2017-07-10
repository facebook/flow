/* @flow */

import { A } from "./A.js"

let a1 = new A;
let a2: A = new A; //ng
let A3: Class<A> = A; //ng
let A4: AbstractClass<A> = A;

let s1 = A.static_m("asdf"); //ng
let s2: string = A.static_m("qwerty"); //ng
