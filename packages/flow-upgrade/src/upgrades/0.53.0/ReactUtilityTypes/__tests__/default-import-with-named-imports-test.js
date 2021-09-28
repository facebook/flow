/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

'use strict';

const path = require('path');
const fs = require('fs');
const cp = require('child_process');
const jscodeshiftFlowParser = require('jscodeshift/parser/flow');
const jscodeshift = require('jscodeshift').withParser(jscodeshiftFlowParser);
const transform = require('../codemod');

const JSCODESHIFT_BIN = path.resolve(
  __dirname,
  '../../node_modules/.bin/jscodeshift',
);

test('default import with named imports', () => {
  const source = `
    import ReactImport, {Component, Element as ReactElement} from 'react';

    class SomeComponent extends Component {
      props: {
        children?: ReactElement<any>
      };
    }
  `;

  const expectedOutput = `
    import * as ReactImport from 'react';

    class SomeComponent extends ReactImport.Component {
      props: {
        children?: ReactImport.Element<any>
      };
    }
  `;

  const root = jscodeshift(source);
  transform(jscodeshift, root);
  expect(root.toSource()).toEqual(expectedOutput);
});
