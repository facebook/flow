/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict
 * @format
 */

'use strict';

export type {AnalyzeOptions, PartialAnalyzeOptions} from './analyze';
export {analyze} from './analyze';
export * from './definition';
export {Reference} from './referencer/Reference';
export * from './scope';
export {ScopeManager} from './ScopeManager';
export * from './variable';
