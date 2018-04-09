/* @flow */

import type {PathNode} from './getPathToLoc';
import type {FlowLoc} from '../flowResult';

export opaque type Context = 'normal' | 'jsx' | 'template';
export const NORMAL: Context = 'normal';
export const JSX: Context = 'jsx';
export const TEMPLATE: Context = 'template';

export default function(loc: FlowLoc, path: Array<PathNode>): [Context, Object /* ast */] {
  let inside: Context = NORMAL
  let ast = path[0].ast;
  for (let i = 0; i < path.length; i++) {
    ast = path[i].ast;
    if (ast.loc && ast.loc.start.line >= loc.start.line) {
      // We've reached the line
      break;
    }

    if (i < path.length - 1 &&
        ast.type === 'JSXElement' &&
        path[i+1].key === 'children') {
      // We've entered a JSX children block
      inside = JSX;
      i++;
    } else if (i < path.length - 1 &&
        ast.type === 'TemplateLiteral' &&
        path[i+1].key === 'expressions') {
      // We've entered a template string
      inside = TEMPLATE;
      i++;
    } else if (inside !== JSX || ast.type != 'JSXText') {
      inside = NORMAL;
    }
  }

  return [inside, ast];
}
