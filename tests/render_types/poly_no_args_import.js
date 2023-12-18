import {Poly, type U, type Multi} from './poly_no_args';
import * as React from 'react';

type T = Poly; // ERROR
type U2 = renders Poly; // OK


const el1: U = <Poly />;
const el2: U2 = <Poly />;
const el3: Multi = <Poly />;

component NotPoly() { return null }
const bad1: U = <NotPoly />; // ERROR
const bad2: U2 = <NotPoly />; // ERROR
const bad3: Multi = <NotPoly />; // ERROR
