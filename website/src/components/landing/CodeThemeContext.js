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
 * Optional Prism theme override consumed by FlowCheckCodeBlock. When null,
 * the code block uses the site's default Prism theme. The landing page sets
 * this so flow-check blocks inside Showcase rows render with a dark theme
 * regardless of the (light) site-wide default.
 */
export const CodeThemeContext: React.Context<mixed> = React.createContext(null);
