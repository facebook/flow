/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

import type {PathNode} from './getPathToLoc';
import type {FlowLoc} from '../flowResult';

export opaque type Context = 'normal' | 'jsx' | 'jsx_fragment' | 'template';
const NORMAL: Context = 'normal';
const JSX: Context = 'jsx';
const JSX_FRAGMENT: Context = 'jsx_fragment';
const TEMPLATE: Context = 'template';

function getContext(
  loc: FlowLoc,
  path: Array<PathNode>,
): [Context, Object /* ast */] {
  let inside: Context = NORMAL;
  let ast = path[0].ast;
  for (let i = 0; i < path.length; i++) {
    ast = path[i].ast;
    if (ast.loc && ast.loc.start.line >= loc.start.line) {
      // We've reached the line
      break;
    }

    if (
      i < path.length - 1 &&
      ast.type === 'JSXElement' &&
      path[i + 1].key === 'children'
    ) {
      // We've entered a JSX children block
      inside = JSX;
      i++;
    } else if (
      i < path.length - 1 &&
      ast.type === 'JSXFragment' &&
      path[i + 1].key === 'children'
    ) {
      // We've entered a JSX fragment block
      inside = JSX_FRAGMENT;
      i++;
    } else if (
      i < path.length - 1 &&
      ast.type === 'TemplateLiteral' &&
      path[i + 1].key === 'expressions'
    ) {
      // We've entered a template string
      inside = TEMPLATE;
      i++;
    } else if (inside !== JSX || ast.type != 'JSXText') {
      inside = NORMAL;
    }
  }

  return [inside, ast];
}

module.exports = {
  NORMAL,
  JSX,
  JSX_FRAGMENT,
  TEMPLATE,
  default: getContext,
};
