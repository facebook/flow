/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import * as monaco from 'monaco-editor';
import {loader} from '@monaco-editor/react';
import type FlowJsServices from './flow-services';
import createTokensProvider from './tokens-theme-provider';
import flowLanguageConfiguration from './flow-configuration.json';

type Position = {lineNumber: number, column: number};

let getDefFunctionForMonaco = (
  value: string,
  position: Position,
): $ReadOnlyArray<FlowLoc> => [];

function setGetDefFunction(flowService: ?FlowJsServices): void {
  getDefFunctionForMonaco = (value, position) =>
    flowService?.getDef?.(
      '-',
      value,
      position.lineNumber,
      position.column - 1,
    ) ?? [];
}

let typeAsPosFunctionForMonaco = (value: string, position: Position): ?string =>
  null;

function setTypeAtPosFunction(flowService: ?FlowJsServices): void {
  typeAsPosFunctionForMonaco = (value, position) =>
    flowService?.typeAtPos(
      '-',
      value,
      position.lineNumber,
      position.column - 1,
    );
}

monaco.languages.register({
  id: 'flow',
  extensions: ['.js', '.flow'],
  aliases: ['Flow'],
});
monaco.languages.setLanguageConfiguration('flow', flowLanguageConfiguration);
const languageId = monaco.languages.getEncodedLanguageId('flow');
monaco.languages.setTokensProvider('flow', createTokensProvider(languageId));
monaco.languages.registerDefinitionProvider('flow', {
  provideDefinition(model, position) {
    try {
      return getDefFunctionForMonaco(model.getValue(), position).map(loc => ({
        uri: model.uri,
        range: {
          startLineNumber: loc.start.line,
          startColumn: loc.start.column,
          endLineNumber: loc.end.line,
          endColumn: loc.end.column + 1,
        },
      }));
    } catch (e) {
      console.error(e);
      return null;
    }
  },
});
monaco.languages.registerHoverProvider('flow', {
  provideHover(model, position) {
    const result = typeAsPosFunctionForMonaco(model.getValue(), position);
    if (result == null) return null;
    // flow.js <= 0.125 incorrectly returned an ocaml string
    // instead of a JS string, where the string value is hidden in a
    // `c` property.
    const typeAtPos = typeof result === 'string' ? result : result.c;
    return {
      contents: [{value: `\`\`\`flow\n${typeAtPos}\n\`\`\``}],
    };
  },
});
loader.config({monaco});

export {monaco, setGetDefFunction, setTypeAtPosFunction};
