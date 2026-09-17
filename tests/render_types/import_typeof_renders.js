import {Poly} from './poly_no_args';
import typeof {Poly as PolyT} from './poly_no_args';
import * as React from 'react';

export type U_direct = renders Poly; // control — OK
export type U_typeof = renders PolyT; // should also be OK

const el1: U_direct = <Poly />; // OK
const el2: U_typeof = <Poly />; // OK

component NotPoly() { return null }
const bad1: U_direct = <NotPoly />; // ERROR
const bad2: U_typeof = <NotPoly />; // ERROR
