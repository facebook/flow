import {num1, str1} from "CJS_Named";
import CJS_Named from "CJS_Named";
num1 as number;
num1 as string; // Error: number ~> string
str1 as string;
str1 as number; // Error: string ~> number
CJS_Named as {num1: number, str1: string};
CJS_Named as number; // Error: Module ~> number

import {num2} from "CJS_Clobbered"; // Error: No such export!
import {numExport} from "CJS_Clobbered";
numExport as number;
numExport as string; // Error: number ~> string
import type {numType} from "CJS_Clobbered";
42 as numType;
'asdf' as numType; // Error: string ~> number

import {strHidden} from "ES"; // Error: No such export!
import {str3} from "ES";
str3 as string;
str3 as number; // Error: string ~> number

import {num3} from "ES";
num3 as number;
num3 as string; // Error: number ~> string

import {C} from "ES";
import type {C as CType} from "ES";
new C() as C;
42 as C; // Error: number ~> C
new C() as CType;
42 as CType; // Error: number ~> CType

import {T} from "ES"; // Error: T is a type import, not a value
import type {T as T2} from "ES";
42 as T2;
'asdf' as T2; // Error: string ~> number

import {exports as nope} from "ES"; // Error: Not an export

import type {Foo} from 're-export'; // Error: imports are not explicitly exported
