// @flow

import {
  type OptionalFunctionField,
  type OptionalComputedMethod,
  type OptionalGenericMethod,
  type OptionalMethod,
  type OptionalReturn,
  genericNarrow,
  narrow,
  optionalNarrow,
  OptionalClassMethod,
} from './optional_methods';

interface RequiredReturn<T> extends OptionalReturn<T> {
  return(value: T): T;
}

const method: OptionalMethod = narrow;
const optionalMethod: OptionalMethod = optionalNarrow;
const genericMethod: OptionalGenericMethod = genericNarrow;
const classMethod: OptionalClassMethod = narrow;
const computedMethod: OptionalComputedMethod = narrow;
const functionField: OptionalFunctionField = narrow; // ERROR

OptionalClassMethod.cb?.({name: 'animal'});
