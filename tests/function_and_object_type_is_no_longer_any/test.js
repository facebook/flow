// @flow

import {
  globalFunction as signatureGlobalFunction,
  globalObject as signatureGlobalObject,
} from './globals';
import {localFunction, localObject} from './locals';

declare const globalFunction: Function;
globalFunction as number; // error: `Function` resolves to the global class

declare const globalObject: Object;
globalObject as number; // error: `Object` resolves to the global class

signatureGlobalFunction as number; // error: type signatures preserve global `Function`
signatureGlobalObject as number; // error: type signatures preserve global `Object`

localFunction as number;
localObject as string;

function locallyDefinedTypes(): void {
  type Function = number;
  type Object = string;

  const localFunction: Function = 42;
  const localObject: Object = 'value';
}
