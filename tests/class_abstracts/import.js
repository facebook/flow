import { A } from "./A.js"

let a1: A = new A; //ng
let A2: Class<A> = A; //ng

let s1: string = A.static_m("qwerty"); //ng
