import {num1, str1} from "CJS_Named";
import CJS_Named from "CJS_Named";
(num1: number);
(num1: string); // Error: number ~> string
(str1: string);
(str1: number); // Error: string ~> number
(CJS_Named: {num1: number, str1: string});
(CJS_Named: number); // Error: Module ~> number

import {num2} from "CJS_Clobbered"; // Error: No such export!
import {numExport} from "CJS_Clobbered";
(numExport: number);
(numExport: string); // Error: number ~> string
import type {numType} from "CJS_Clobbered";
(42: numType);
('asdf': numType); // Error: string ~> number

import {strHidden} from "ES"; // Error: No such export!
import {str3} from "ES";
(str3: string);
(str3: number); // Error: string ~> number

import {num3} from "ES";
(num3: number);
(num3: string); // Error: number ~> string

import {C} from "ES";
import type {C as CType} from "ES";
(new C(): C);
(42: C); // Error: number ~> C
(new C(): CType);
(42: CType); // Error: number ~> CType

import {T} from "ES"; // Error: T is a type import, not a value
import type {T as T2} from "ES";
(42: T2);
('asdf': T2); // Error: string ~> number

import {exports as nope} from "ES"; // Error: Not an export

import type {Foo} from 're-export'; // Error: imports are not explicitly exported
