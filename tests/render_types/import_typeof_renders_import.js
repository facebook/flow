import type {U_cjs_getter, U_typeof} from './import_typeof_renders';
import * as React from 'react';

component NotPoly() {
  return null;
}

const badTypeof: U_typeof = <NotPoly />; // ERROR
const badCJSGetter: U_cjs_getter = <NotPoly />; // ERROR
