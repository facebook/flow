// @flow

import {take} from './cyclic_type_import_union-take';
import X from './cyclic_type_import_union-X';
import Y from './cyclic_type_import_union-Y';

// $FlowFixMe[signature-verification-failure]
const x = new X();
// $FlowFixMe[signature-verification-failure]
const y = new Y();

export type XInstance = typeof x;
export type XValidationFunction = ReturnType<XInstance['compile']>;

export type YInstance = typeof y;
export type YValidationFunction = {
  (): boolean,
  errors?: ?Array<string>,
  schema?: string,
  ...
};

export type XPackage = {
  instance: XInstance,
  validate?: XValidationFunction,
};

export type YPackage = {
  instance: YInstance,
  validate?: YValidationFunction,
};

declare const cond: boolean;

const pkg: XPackage | YPackage = cond
  ? {
      instance: y,
      validate: y.compile(),
    }
  : {
      instance: x,
      validate: x.compile(),
    };

take(pkg);
