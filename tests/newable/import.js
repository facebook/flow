// @flow

import {E, Ln} from "./class_function"

let L = Ln(E); //ok
//let ell1 = new L({x:21, y:22}); //ok
let ell2 = new L("not an object");
