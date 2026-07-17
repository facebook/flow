/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict
 * @format
 */

/*
Unfortunately flow does not strictly enforce generic constraints for a type like
type T = <T>(target: T, nodeToReplaceWith: T) => void;

When called, it will set T to the union of argument types instead of enforcing they
are the same type.
So in order to have a strict type guarantee that both arguments are the same type -
we need codegen!
*/

import {
  GetTransformESTreeJSON,
  TransformESTreePackage,
  TransformPackage,
  formatAndWriteDistArtifact,
  LITERAL_TYPES,
} from './utils/scriptUtils';

const imports: Array<string> = [];
const replaceSignatures: Array<string> = [];

const nodes = GetTransformESTreeJSON().concat({
  name: 'Literal',
  arguments: [],
});
const signatureNames: Array<string> = [];
for (const node of nodes) {
  if (LITERAL_TYPES.has(node.name)) {
    continue;
  }

  imports.push(node.name);
  const signatureName = `${node.name}ReplaceSignature`;
  signatureNames.push(signatureName);

  replaceSignatures.push(
    `type ${signatureName} = (
      target: ${node.name},
      nodeToReplaceWith: DetachedNode<${node.name}>,
      options?: Readonly<{keepComments?: boolean}>,
    ) => void`,
  );
}

const fileContents = `\
import type {
${imports.join(',\n')}
} from '${TransformESTreePackage}';
import type {DetachedNode} from '../detachedNode';

${replaceSignatures.join(';\n')};
export type TransformReplaceSignatures = ${signatureNames.join(' & ')};
`;

formatAndWriteDistArtifact({
  code: fileContents,
  package: TransformPackage,
  file: 'generated/TransformReplaceSignatures.js.flow',
  flow: 'strict-local',
});
