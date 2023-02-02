/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import {INITIAL, Registry, parseRawGrammar} from 'vscode-textmate';
import {createOnigScanner, createOnigString, loadWASM} from 'vscode-oniguruma';
import flowGrammar from './flow-grammar.json';
import flowRegexGrammar from './flow-regex-grammar.json';
import THEME from './light_vs';
import styles from './TryFlow.module.css';

const grammars = {
  'source.js': flowGrammar,
  'source.regexp.flow': flowRegexGrammar,
};

// $FlowFixMe[cannot-resolve-module]
const registry = import('vscode-oniguruma/release/onig.wasm')
  .then(wasmModule => fetch(wasmModule.default))
  // manually convert to an ArrayBuffer because Jekyll 3.x doesn't
  // support serving .wasm as application/wasm via `jekyll serve`.
  // Fixed in Jekyll 4
  .then(response => response.arrayBuffer())
  .then(data => loadWASM(data))
  .then(() => {
    return new Registry({
      onigLib: Promise.resolve({createOnigScanner, createOnigString}),
      loadGrammar: scopeName => grammars[scopeName],
      theme: THEME,
    });
  })
  .then(registry => {
    const headNode = document.getElementsByTagName('head')[0];
    const styles = document.createElement('style');
    styles.type = 'text/css';
    styles.media = 'screen';
    styles.className = 'monaco-colors';
    headNode.appendChild(styles);

    const colors = registry.getColorMap();
    styles.innerHTML = generateTokensCSSForColorMap(colors);
    return registry;
  });

// from https://github.com/microsoft/vscode/blob/013501950e78b9dde5c2e6ec3f2ddfb9201156b7/src/vs/editor/common/modes/supports/tokenization.ts#L398
// $FlowFixMe[missing-local-annot]
function generateTokensCSSForColorMap(colorMap) {
  const rules = [];
  for (let i = 1, len = colorMap.length; i < len; i++) {
    let color = colorMap[i];
    // CUSTOM: .code is Try Flow's parent component. we make it more specific to override Monaco
    rules[i] = `.${styles.code} .mtk${i} { color: ${color}; }`;
  }
  rules.push(`.${styles.code}  .mtki { font-style: italic; }`);
  rules.push(`.${styles.code}  .mtkb { font-weight: bold; }`);
  rules.push(
    `.${styles.code}  .mtku { text-decoration: underline; text-underline-position: under; }`,
  );
  return rules.join('\n');
}

export default function createTokensProvider(languageId: string): Promise<any> {
  return registry
    .then(registry =>
      registry.loadGrammarWithConfiguration('source.js', languageId, {}),
    )
    .then(grammar => {
      if (grammar == null) {
        throw Error(`no grammar for ${languageId}`);
      }

      return {
        getInitialState() {
          return INITIAL;
        },

        tokenizeEncoded(line, state) {
          const tokenizeLineResult2 = grammar.tokenizeLine2(line, state);
          const endState = tokenizeLineResult2.ruleStack;
          const {tokens} = tokenizeLineResult2;
          return {tokens, endState};
        },
      };
    });
}
