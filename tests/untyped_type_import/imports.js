/* @flow */

/* Named Imports from Untyped File */

import type {FooType} from './untyped_exports.js'; //Error
import typeof {FooObj} from './untyped_exports.js'; //Error

/* Default Imports from Untyped File */

import typeof BarObj from './untyped_exports.js'; //Error

/* Named Imports from Untyped File (That Just Don't Exist) */

import type {BazType} from './untyped_exports.js'; //Error
import typeof {BazObj} from './untyped_exports.js'; //Error

/* ``Any`` Imports from Typed File */
import type {AnyType} from './typed_exports.js' //Not an error
import typeof {AnyObj} from './typed_exports.js' //Not an error
import typeof AnyObjDefault from './typed_exports.js' //Not an error

/* Suppressed Imports */
/* flowlint untyped-type-import:off */

import type {BeepType} from './untyped_exports.js'; //Error; Suppressed
import typeof {BeepObj} from './untyped_exports.js'; //Error; Suppressed

import typeof BoopObj from './untyped_exports.js'; //Error; Suppressed
