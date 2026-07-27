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

import {
  GetFlowESTreeJSON,
  formatAndWriteDistArtifact,
  LITERAL_TYPES,
} from './transform/utils/scriptUtils';

const imports: Array<string> = ['ESNode'];
const enterSelectors: Array<string> = [];
const exitSelectors: Array<string> = [];
const typeAliases = new Map<string, string>();

const FlowESTreeJSON = GetFlowESTreeJSON();

function pushSelector(selector: string, type: string): void {
  enterSelectors.push(`readonly '${selector}'?: (node: ${type}) => void`);
  exitSelectors.push(`readonly '${selector}:exit'?: (node: ${type}) => void`);
}

for (const node of FlowESTreeJSON.concat({name: 'Literal', arguments: []})) {
  if (LITERAL_TYPES.has(node.name)) {
    continue;
  }

  imports.push(node.name);

  // add the standard single-node selectors
  pushSelector(node.name, node.name);

  // add a selector for each of the simple cases of checking for property existence
  for (const arg of node.arguments) {
    const typeAliasName = `${node.name}_With_${arg.name}`;
    typeAliases.set(
      typeAliasName,
      // This creates an "override" type that enforces that the property exists and is not nullish
      // interface VariableDeclarator_With_Init VariableDeclarator { readonly init: NonNullable<VariableDeclarator['init']> }
      `interface ${typeAliasName} extends ${node.name} { readonly ${arg.name}: NonNullable<${node.name}['${arg.name}']> }`,
    );
    pushSelector(`${node.name}[${arg.name}]`, typeAliasName);
  }
}

// add special selectors
const specialSelectors = [
  {
    selector: '*',
    aliasName: 'Star',
    type: ['ESNode'],
  },
  // https://github.com/estools/esquery/blob/7c3800a4b2ff5c7b3eb3b2cf742865b7c908981f/esquery.js#L211-L234
  {
    selector: ':statement',
    aliasName: 'Statement',
    type: getNodeNamesThatEndWith('Statement'),
  },
  {
    selector: ':declaration',
    aliasName: 'Declaration',
    type: getNodeNamesThatEndWith('Declaration'),
  },
  {
    selector: ':pattern',
    aliasName: 'Pattern',
    type: getNodeNamesThatEndWith('Pattern'),
  },
  {
    selector: ':expression',
    aliasName: 'Expression',
    type: ['Identifier', 'MetaProperty']
      .concat(getNodeNamesThatEndWith('Expression'))
      .concat(getNodeNamesThatEndWith('Literal')),
  },
  {
    selector: ':function',
    aliasName: 'Function',
    type: [
      'FunctionDeclaration',
      'FunctionExpression',
      'ArrowFunctionExpression',
    ],
  },
];

for (const specialSelector of specialSelectors) {
  const typeAliasName = `${specialSelector.aliasName}SpecialSelector`;
  typeAliases.set(
    typeAliasName,
    `type ${typeAliasName} = ${specialSelector.type.join(' | ')}`,
  );
  pushSelector(specialSelector.selector, typeAliasName);
}

const fileContents = `\
import type {
${imports.join(',\n')}
} from '../types';

${Array.from(typeAliases.values()).join('\n')}

export type ESQueryNodeSelectorsWithoutFallback = {
${enterSelectors.join(',\n')},
${exitSelectors.join(',\n')},
};
`;

// TODO: Move to `src` directory, currently causes Flow errors.
formatAndWriteDistArtifact({
  code: fileContents,
  package: 'flow-estree',
  file: 'generated/HermesESTreeSelectorTypes.js.flow',
  flow: 'strict',
});

function getNodeNamesThatEndWith(name: string): Array<string> {
  const names = [];
  for (const node of FlowESTreeJSON) {
    if (LITERAL_TYPES.has(node.name)) {
      continue;
    }

    if (node.name.endsWith(name)) {
      names.push(node.name);
    }
  }
  return names;
}
