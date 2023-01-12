/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import React, {useEffect, useRef, type MixedElement} from 'react';
import * as monaco from 'monaco-editor';
import Editor, {loader} from '@monaco-editor/react';
import * as LZString from 'lz-string';

import createTokensProvider from './tokens-theme-provider';
import flowLanguageConfiguration from './flow-configuration.json';
import type {AsyncFlow} from './init-flow';

function getHashedValue(hash) {
  if (hash[0] !== '#' || hash.length < 2) return null;
  const version = hash.slice(1, 2);
  const encoded = hash.slice(2);
  if (version === '0' && encoded.match(/^[a-zA-Z0-9+/=_-]+$/)) {
    return LZString.decompressFromEncodedURIComponent(encoded);
  }
  return null;
}

const lastEditorValue = localStorage.getItem('tryFlowLastContent');
const defaultValue =
  (lastEditorValue && getHashedValue(lastEditorValue)) ||
  `/* @flow */

function foo(x: ?number): string {
  if (x) {
    return x;
  }
  return "default string";
}
`;

monaco.languages.register({
  id: 'flow',
  extensions: ['.js', '.flow'],
  aliases: ['Flow'],
});
monaco.languages.setLanguageConfiguration('flow', flowLanguageConfiguration);
const languageId = monaco.languages.getEncodedLanguageId('flow');
monaco.languages.setTokensProvider('flow', createTokensProvider(languageId));
loader.config({monaco});

export {monaco};

type Props = {
  editorRef: {current: any},
  flowRef: {current: Promise<AsyncFlow>},
  onCodeChange: () => void,
};

export default function TryFlowEditor({
  editorRef,
  flowRef,
  onCodeChange,
}: Props): MixedElement {
  const providerRef = useRef();

  useEffect(() => {
    return () => providerRef.current?.dispose();
  }, []);

  return (
    <Editor
      defaultValue={getHashedValue(location.hash) || defaultValue}
      defaultLanguage="flow"
      theme="vs-light"
      height="calc(100vh - var(--ifm-navbar-height))"
      onChange={onCodeChange}
      options={{
        minimap: {enabled: false},
        hover: {enabled: true, above: false},
        scrollBeyondLastLine: false,
        overviewRulerBorder: false,
      }}
      onMount={editor => {
        editorRef.current = editor;
        onCodeChange();
      }}
      beforeMount={monaco => {
        const provider = monaco.languages.registerHoverProvider('flow', {
          provideHover(model, position) {
            return flowRef.current
              .then(flowProxy => {
                const value = monaco.editor.getModels()[0].getValue();
                return flowProxy.typeAtPos(
                  '-',
                  value,
                  position.lineNumber,
                  position.column - 1,
                );
              })
              .catch(e => null)
              .then(result => {
                if (result == null) return null;
                // flow.js <= 0.125 incorrectly returned an ocaml string
                // instead of a JS string, where the string value is hidden in a
                // `c` property.
                const typeAtPos =
                  typeof result === 'string' ? result : result.c;
                return {contents: [{value: typeAtPos}]};
              });
          },
        });
        providerRef.current = provider;
      }}
    />
  );
}
