import * as React from 'react';
export component Poly<T>() { return null }

type T = Poly; // ERROR
export type U = renders Poly; // OK!

component Poly2<T>() { return null }
export type Multi = renders (Poly | Poly2);

const el1: U = <Poly />; // OK
const el2: Multi = <Poly />; // OK
const el3: Multi = <Poly2 />; // OK

component NotPoly() { return null }
const bad1: U = <NotPoly />; // ERROR
const bad2: Multi = <NotPoly />; // ERROR
