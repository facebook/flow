// @flow

import type {HasLength} from './boxing_lib';

declare const value: string;
value as HasLength; // OK: interface comes from .ts
