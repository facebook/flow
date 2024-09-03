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

let autoCompleteFunctionForMonaco = (
  value: string,
  position: Position,
): any => {
  throw JSON.stringify({position, error: 'not implemented'});
};

function setAutoCompleteFunction(flowService: ?FlowJsServices): void {
  autoCompleteFunctionForMonaco = (value, position) =>
    flowService?.autocomplete?.(
      '-',
      value,
      position.lineNumber,
      position.column - 1,
    ) ?? {incomplete: false, suggestions: []};
}

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

let typeAsPosFunctionForMonaco = (
  value: string,
  position: Position,
): ?string | Array<{type: 'flow' | 'markdown', value: string}> => null;

function setTypeAtPosFunction(flowService: ?FlowJsServices): void {
  typeAsPosFunctionForMonaco = (value, position) =>
    flowService?.typeAtPos(
      '-',
      value,
      position.lineNumber,
      position.column - 1,
    );
}

let signatureHelpFunctionForMonaco = (value: string, position: Position): any =>
  null;

function setSignatureHelpFunction(flowService: ?FlowJsServices): void {
  signatureHelpFunctionForMonaco = (value, position) =>
    flowService?.signatureHelp(
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

monaco.languages.registerCompletionItemProvider('flow', {
  triggerCharacters: [
    '.',
    'A',
    'B',
    'C',
    'D',
    'E',
    'F',
    'G',
    'H',
    'I',
    'J',
    'K',
    'L',
    'M',
    'N',
    'O',
    'P',
    'Q',
    'R',
    'S',
    'T',
    'U',
    'V',
    'W',
    'X',
    'Y',
    'Z',
    'a',
    'b',
    'c',
    'd',
    'e',
    'f',
    'g',
    'h',
    'i',
    'j',
    'k',
    'l',
    'm',
    'n',
    'o',
    'p',
    'q',
    'r',
    's',
    't',
    'u',
    'v',
    'w',
    'x',
    'y',
    'z',
    '0',
    '1',
    '2',
    '3',
    '4',
    '5',
    '6',
    '7',
    '8',
    '9',
    '[',
    '"',
    "'",
  ],

  provideCompletionItems(model, position) {
    try {
      const result = autoCompleteFunctionForMonaco(model.getValue(), position);
      return result;
    } catch (e) {
      console.error(e);
      return null;
    }
  },
});
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
    function markdownValue(value: string, type: 'flow' | 'markdown' = 'flow') {
      return value.startsWith('type_repr: ')
        ? {
            value: `\`\`\`ocaml\n${value.substring('type_repr: '.length)}\n\`\`\``,
          }
        : type === 'markdown'
          ? {value}
          : {value: `\`\`\`${type}\n${value}\n\`\`\``};
    }
    if (typeof result === 'string') {
      return {
        contents: [markdownValue(result)],
      };
    }
    return {contents: result.map(r => markdownValue(r.value, r.type))};
  },
});
monaco.languages.registerSignatureHelpProvider('flow', {
  signatureHelpTriggerCharacters: ['(', ',', '{'],
  provideSignatureHelp(model, position) {
    try {
      const result = signatureHelpFunctionForMonaco(model.getValue(), position);
      if (result == null) return null;
      return {value: result, dispose() {}};
    } catch (e) {
      console.error(e);
      return null;
    }
  },
});
loader.config({monaco});

export {
  monaco,
  setAutoCompleteFunction,
  setGetDefFunction,
  setTypeAtPosFunction,
  setSignatureHelpFunction,
};
