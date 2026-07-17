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

import fs from 'fs';
import mkdirp from 'mkdirp';
import path from 'path';
import * as prettier from 'prettier';
// $FlowExpectedError[cannot-resolve-module]
import prettierConfig from '../../../.prettierrc.json';

export type ESTreeJSON = ReadonlyArray<
  Readonly<{
    name: string,
    base: string,
    arguments: ReadonlyArray<
      Readonly<{
        type:
          | 'NodeLabel'
          | 'NodeString'
          | 'NodeBoolean'
          | 'NodeNumber'
          | 'NodePtr'
          | 'NodeList',
        name: string,
        optional: boolean,
      }>,
    >,
  }>,
>;

const FLOW_ESTREE_JSON_FILE = path.resolve(__dirname, '../FlowESTreeJSON.json');

export const GetTransformESTreeJSON: () => ESTreeJSON = () =>
  // $FlowExpectedError[unsupported-syntax]
  require(FLOW_ESTREE_JSON_FILE);

export const TransformESTreePackage: 'flow-estree' = 'flow-estree';
export const TransformPackage: 'flow-transform' = 'flow-transform';
export const TransformReadonly: '+' = '+';

type FlowStyle = false | 'loose' | 'strict' | 'strict-local';
function header(flow: FlowStyle, skipFormat: boolean): string {
  let flowDirective = `${'@'}flow`;
  if (flow === false) {
    flowDirective = `${'@'}noflow`;
  } else if (flow !== 'loose') {
    flowDirective += ` ${flow}`;
  }
  const formatDirective = skipFormat ? '' : '\n * @format';

  return `${'/**'}
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * ${flowDirective}${formatDirective}
 * ${'@'}generated
 */

/*
 * !!! GENERATED FILE !!!
 *
 * Any manual changes to this file will be overwritten. To regenerate run \`yarn build\`.
 */

// lint directives to let us do some basic validation of generated files
/* eslint no-undef: 'error', no-unused-vars: ['error', {vars: "local"}], no-redeclare: 'error' */
/* global NonNullable, Partial, Readonly, ReadonlyArray, $FlowFixMe */

'use strict';

`;
}

type ArtifactOptions = Readonly<{
  code: string,
  flow?: FlowStyle,
  package: 'flow-transform',
  file: string,
  skipFormat?: boolean,
}>;

export async function formatAndWriteDistArtifact(
  opts: ArtifactOptions,
): Promise<void> {
  await formatAndWriteArtifact({
    ...opts,
    file: path.join('dist', opts.file),
  });
}

export async function formatAndWriteSrcArtifact(
  opts: ArtifactOptions,
): Promise<void> {
  await formatAndWriteArtifact({
    ...opts,
    file: path.join('src', opts.file),
  });
}

async function formatAndWriteArtifact({
  code: unformattedCode,
  flow = 'loose',
  package: packageName,
  file,
  skipFormat = false,
}: ArtifactOptions): Promise<void> {
  const code =
    unformattedCode.slice(0, 3) === '/**'
      ? unformattedCode
      : header(flow, skipFormat) + unformattedCode;
  const formattedContents = skipFormat
    ? code
    : await prettier.format(code, {
        ...prettierConfig,
        parser: 'flow',
      });

  const packageRoot = path.resolve(__dirname, '..', '..', '..', packageName);
  const folder = path.resolve(packageRoot, path.dirname(file));
  mkdirp.sync(folder);
  fs.writeFileSync(
    path.resolve(folder, path.basename(file)),
    formattedContents,
  );
}

export const LITERAL_TYPES: ReadonlySet<string> = new Set([
  'BigIntLiteral',
  'BooleanLiteral',
  'NullLiteral',
  'NumericLiteral',
  'RegExpLiteral',
  'StringLiteral',
]);

export const EXCLUDE_PROPERTIES_FROM_NODE: ReadonlyMap<
  string,
  ReadonlySet<string>,
> = new Map([['PropertyDefinition', new Set(['tsModifiers'])]]);
