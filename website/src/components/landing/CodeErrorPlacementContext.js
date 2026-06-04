/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import * as React from 'react';

/**
 * Where FlowCheckCodeBlock renders Flow errors. Default ("inline") inserts
 * an error row directly under each offending line of code — useful in docs
 * where the error is the lesson. The landing sets this to "end" so the code
 * itself reads clean, and errors are summarized as a footer block below the
 * snippet.
 */
export const CodeErrorPlacementContext: React.Context<'inline' | 'end'> =
  React.createContext('inline');
