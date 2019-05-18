// @flow

declare function lodash_isNil(value: mixed): boolean %checks(value == null)

declare class Lodash {
  +isNil: typeof lodash_isNil;
}

declare export var _: Lodash;
