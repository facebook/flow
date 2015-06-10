/* @flow */

function takesANumber(num: number): void {}
function takesAString(str: string): void {}

// ===================== //
// == Path Resolution == //
// ===================== //

// @providesModule
import DefaultA from "A"; // Error: There is no default export
import * as DefaultA from "A";
takesANumber(DefaultA.numberValue1);
takesAString(DefaultA.numberValue1); // Error: number ~> string

// File path
import * as DefaultB from "./B";
takesANumber(DefaultB.numberValue);
takesAString(DefaultB.numberValue); // Error: number ~> string

// C.js exists, but not as a providesModule
import DefaultC from "C"; // Error: No such module

// @providesModule D exists, but not as a filename
import DefaultD from "./D"; // Error: No such module

// ================================================ //
// == CommonJS Clobbering Literal Exports -> ES6 == //
// ================================================ //

import {doesntExist} from "CommonJS_Clobbering_Lit"; // Error: Not an exported binding

import {numberValue1} from "CommonJS_Clobbering_Lit";
takesANumber(numberValue1);
takesAString(numberValue1); // Error: number ~> string

import {numberValue2 as numVal1} from "CommonJS_Clobbering_Lit";
takesANumber(numVal1);
takesAString(numVal1); // Error: number ~> string

import CJS_Clobb_Lit from "CommonJS_Clobbering_Lit";
takesANumber(CJS_Clobb_Lit.numberValue3);
takesAString(CJS_Clobb_Lit.numberValue3); // Error: number ~> string
takesANumber(CJS_Clobb_Lit.doesntExist); // Error: doesntExist isn't a property

import * as CJS_Clobb_Lit_NS from "CommonJS_Clobbering_Lit";
takesANumber(CJS_Clobb_Lit_NS.numberValue4);
takesANumber(CJS_Clobb_Lit_NS.default.numberValue4);
CJS_Clobb_Lit_NS.default.default; // Error: No 'default' property on the exported obj
takesAString(CJS_Clobb_Lit_NS.numberValue4); // Error: number ~> string
takesAString(CJS_Clobb_Lit_NS.default.numberValue5); // Error: number ~> string

// ============================================== //
// == CommonJS Clobbering Class Exports -> ES6 == //
// ============================================== //

import {doesntExist} from "CommonJS_Clobbering_Class"; // Error: Not an exported binding

import {staticNumber1} from "CommonJS_Clobbering_Class";
takesANumber(staticNumber1());
takesAString(staticNumber1()); // Error: number ~> string

import CJS_Clobb_Class from "CommonJS_Clobbering_Class";
new CJS_Clobb_Class();
new CJS_Clobb_Class().doesntExist; // Error: Class has no `doesntExist` property
takesANumber(CJS_Clobb_Class.staticNumber2());
takesAString(CJS_Clobb_Class.staticNumber2()); // Error: number ~> string
takesANumber(new CJS_Clobb_Class().instNumber1());
takesAString(new CJS_Clobb_Class().instNumber1()); // Error: number ~> string

import * as CJS_Clobb_Class_NS from "CommonJS_Clobbering_Class";
new CJS_Clobb_Class_NS(); // Error: Namespace object isn't constructable
takesANumber(CJS_Clobb_Class_NS.staticNumber3());
takesAString(CJS_Clobb_Class_NS.staticNumber3()); // Error: number ~> string
new CJS_Clobb_Class_NS.default();
takesANumber(CJS_Clobb_Class_NS.default.staticNumber4());
takesAString(CJS_Clobb_Class_NS.default.staticNumber4()); // Error: number ~> string
takesANumber(new CJS_Clobb_Class_NS.default().instNumber2());
takesAString(new CJS_Clobb_Class_NS.default().instNumber2()); // Error: number ~> string

// =================================== //
// == CommonJS Named Exports -> ES6 == //
// =================================== //

import {doesntExist} from "CommonJS_Named"; // Error: Not an exported binding

import {numberValue2} from "CommonJS_Named";
takesANumber(numberValue2);
takesAString(numberValue2); // Error: number ~> string

import {numberValue3 as numVal3} from "CommonJS_Named";
takesANumber(numVal3);
takesAString(numVal3); // Error: number ~> string

import * as CJS_Named from "CommonJS_Named";
takesANumber(CJS_Named.numberValue1);
takesAString(CJS_Named.numberValue1); // Error: number ~> string
takesANumber(CJS_Named.doesntExist); // Error: doesntExist isn't a property

import * as CJS_Named_NS from "CommonJS_Named";
takesANumber(CJS_Named_NS.numberValue4);
takesANumber(CJS_Named_NS.default.numberValue4); // Error: CommonJS_Named has no default export
takesAString(CJS_Named_NS.numberValue4); // Error: number ~> string

//////////////////////////////
// == ES6 Default -> ES6 == //
//////////////////////////////

import {doesntExist} from "ES6_Default_AnonFunction1"; // Error: Not an exported binding

import ES6_Def_AnonFunc1 from "ES6_Default_AnonFunction1";
takesANumber(ES6_Def_AnonFunc1());
takesAString(ES6_Def_AnonFunc1()); // Error: number ~> string

import ES6_Def_NamedFunc1 from "ES6_Default_NamedFunction1";
takesANumber(ES6_Def_NamedFunc1());
takesAString(ES6_Def_NamedFunc1()); // Error: number ~> string

import ES6_Def_AnonClass1 from "ES6_Default_AnonClass1";
takesANumber(new ES6_Def_AnonClass1().givesANum());
takesAString(new ES6_Def_AnonClass1().givesANum()); // Error: number ~> string

import ES6_Def_NamedClass1 from "ES6_Default_NamedClass1";
takesANumber(new ES6_Def_NamedClass1().givesANum());
takesAString(new ES6_Def_NamedClass1().givesANum()); // Error: number ~> string

////////////////////////////
// == ES6 Named -> ES6 == //
////////////////////////////

import doesntExist from "ES6_Named1"; // Error: Not an exported binding

import {specifierNumber1} from "ES6_Named1";
takesANumber(specifierNumber1);
takesAString(specifierNumber1); // Error: number ~> string

import {specifierNumber2Renamed} from "ES6_Named1";
takesANumber(specifierNumber2Renamed);
takesAString(specifierNumber2Renamed); // Error: number ~> string

import {specifierNumber3 as specifierNumber3Renamed} from "ES6_Named1";
takesANumber(specifierNumber3Renamed);
takesAString(specifierNumber3Renamed); // Error: number ~> string

import {groupedSpecifierNumber1, groupedSpecifierNumber2} from "ES6_Named1";
takesANumber(groupedSpecifierNumber1);
takesANumber(groupedSpecifierNumber2);
takesAString(groupedSpecifierNumber1); // Error: number ~> string
takesAString(groupedSpecifierNumber2); // Error: number ~> string

import {givesANumber} from "ES6_Named1";
takesANumber(givesANumber());
takesAString(givesANumber()); // Error: number ~> string

import {NumberGenerator} from "ES6_Named1";
takesANumber(new NumberGenerator().givesANumber());
takesAString(new NumberGenerator().givesANumber()); // Error: number ~> string

import {varDeclNumber1, varDeclNumber2} from "ES6_Named1";
takesANumber(varDeclNumber1);
takesANumber(varDeclNumber2);
takesAString(varDeclNumber1); // Error: number ~> string
takesAString(varDeclNumber2); // Error: number ~> string

import {destructuredObjNumber} from "ES6_Named1";
takesANumber(destructuredObjNumber);
takesAString(destructuredObjNumber); // Error: number ~> string

import {destructuredArrNumber} from "ES6_Named1";
takesANumber(destructuredArrNumber);
takesAString(destructuredArrNumber); // Error: number ~> string

import {numberValue1 as numberValue4} from "ES6_ExportFrom_Intermediary1";
takesANumber(numberValue4);
takesAString(numberValue4); // Error: number ~> string

import {numberValue2_renamed} from "ES6_ExportFrom_Intermediary1";
takesANumber(numberValue2_renamed);
takesAString(numberValue2_renamed); // Error: number ~> string

import {numberValue1 as numberValue5} from "ES6_ExportAllFrom_Intermediary1";
takesANumber(numberValue5);
takesAString(numberValue5); // Error: number ~> string

///////////////////////////////////
// == ES6 Default -> CommonJS == //
///////////////////////////////////

require('ES6_Default_AnonFunction2').doesntExist; // Error: 'doesntExist' isn't an export

var ES6_Def_AnonFunc2 = require("ES6_Default_AnonFunction2").default;
takesANumber(ES6_Def_AnonFunc2());
takesAString(ES6_Def_AnonFunc2()); // Error: number ~> string

var ES6_Def_NamedFunc2 = require("ES6_Default_NamedFunction2").default;
takesANumber(ES6_Def_NamedFunc2());
takesAString(ES6_Def_NamedFunc2()); // Error: number ~> string

var ES6_Def_AnonClass2 = require("ES6_Default_AnonClass2").default;
takesANumber(new ES6_Def_AnonClass2().givesANum());
takesAString(new ES6_Def_AnonClass2().givesANum()); // Error: number ~> string

var ES6_Def_NamedClass2 = require("ES6_Default_NamedClass2").default;
takesANumber(new ES6_Def_NamedClass2().givesANum());
takesAString(new ES6_Def_NamedClass2().givesANum());

/////////////////////////////////
// == ES6 Named -> CommonJS == //
/////////////////////////////////

var specifierNumber4 = require("ES6_Named2").specifierNumber4;
takesANumber(specifierNumber4);
takesAString(specifierNumber4); // Error: number ~> string

var specifierNumber5Renamed = require("ES6_Named2").specifierNumber5Renamed;
takesANumber(specifierNumber5Renamed);
takesAString(specifierNumber5Renamed); // Error: number ~> string

var groupedSpecifierNumber3 = require("ES6_Named2").groupedSpecifierNumber3;
var groupedSpecifierNumber4 = require("ES6_Named2").groupedSpecifierNumber4;
takesANumber(groupedSpecifierNumber3);
takesANumber(groupedSpecifierNumber4);
takesAString(groupedSpecifierNumber3); // Error: number ~> string
takesAString(groupedSpecifierNumber4); // Error: number ~> string

var givesANumber2 = require("ES6_Named2").givesANumber2;
takesANumber(givesANumber2());
takesAString(givesANumber2()); // Error: number ~> string

var NumberGenerator2 = require("ES6_Named2").NumberGenerator2;
takesANumber(new NumberGenerator2().givesANumber());
takesAString(new NumberGenerator2().givesANumber()); // Error: number ~> string

var varDeclNumber3 = require("ES6_Named2").varDeclNumber3;
var varDeclNumber4 = require("ES6_Named2").varDeclNumber4;
takesANumber(varDeclNumber3);
takesANumber(varDeclNumber4);
takesAString(varDeclNumber3); // Error: number ~> string
takesAString(varDeclNumber4); // Error: number ~> string

var destructuredObjNumber2 = require("ES6_Named2").destructuredObjNumber2;
takesANumber(destructuredObjNumber2);
takesAString(destructuredObjNumber2); // Error: number ~> string

var destructuredArrNumber2 = require("ES6_Named2").destructuredArrNumber2;
takesANumber(destructuredArrNumber2);
takesAString(destructuredArrNumber2); // Error: number ~> string

var numberValue6 = require("ES6_ExportFrom_Intermediary2").numberValue1;
takesANumber(numberValue6);
takesAString(numberValue6); // Error: number ~> string

var numberValue2_renamed2 = require("ES6_ExportFrom_Intermediary2").numberValue2_renamed2;
takesANumber(numberValue2_renamed2);
takesAString(numberValue2_renamed2); // Error: number ~> string

var numberValue7 = require("ES6_ExportAllFrom_Intermediary2").numberValue1;
takesANumber(numberValue7);
takesAString(numberValue7); // Error: number ~> string
