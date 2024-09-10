/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import React, {useState, useEffect, useRef, type MixedElement} from 'react';
import {useBaseUrlUtils} from '@docusaurus/useBaseUrl';
import clsx from 'clsx';
import Editor from '@monaco-editor/react';
import * as LZString from 'lz-string';
import styles from './TryFlow.module.css';
import TryFlowConfigEditor from './TryFlowConfigEditor';
import TryFlowResults from './TryFlowResults';
import {
  monaco,
  setAutoCompleteFunction,
  setGetDefFunction,
  setTypeAtPosFunction,
  setSignatureHelpFunction,
} from './configured-monaco';
import FlowJsServices from './flow-services';
import createTokensProvider from './tokens-theme-provider';
import flowLanguageConfiguration from './flow-configuration.json';

const TRY_FLOW_LAST_CONTENT_STORAGE_KEY = 'tryFlowLastContent';
const DEFAULT_FLOW_PROGRAM = `
function foo(x: ?number): string {
  if (x) {
    return x; // Error: number is not a string
    // Fix: return String(x);
  }
  return "default string";
}
`;

type InitialStateFromHash = {
  code: string,
  config: ?{[string]: mixed},
  version: ?string,
};

function getHashedValue(hash: ?string): ?InitialStateFromHash {
  if (hash == null) return null;
  if (hash[0] !== '#' || hash.length < 2) return null;
  const version = hash.slice(1, 2);
  const encoded = hash.slice(2);
  if (version === '0' && encoded.match(/^[a-zA-Z0-9+/=_-]+$/)) {
    return {
      code: LZString.decompressFromEncodedURIComponent(encoded),
      config: null,
      version: null,
    };
  }
  if (version === '1' && encoded.match(/^[a-zA-Z0-9+/=_-]+$/)) {
    try {
      const {code, config, version} = JSON.parse(
        LZString.decompressFromEncodedURIComponent(encoded),
      );
      if (
        typeof code === 'string' &&
        (config == null || typeof config === 'object') &&
        (version == null || typeof version === 'string')
      ) {
        return {code, config, version};
      }
    } catch {}
  }
  return null;
}

function setHashedValue(
  flowService: ?FlowJsServices,
  version: string,
  code: string,
) {
  const compressed = LZString.compressToEncodedURIComponent(
    JSON.stringify({config: flowService?.config, code, version}),
  );
  window.location.hash = `1${compressed}`;
  localStorage.setItem(TRY_FLOW_LAST_CONTENT_STORAGE_KEY, location.hash);
}

const initialStateFromURI = getHashedValue(location.hash);
const initialState: InitialStateFromHash = initialStateFromURI || {
  // Only default to an example if we haven't used Try Flow before
  code:
    localStorage.getItem(TRY_FLOW_LAST_CONTENT_STORAGE_KEY) != null
      ? ''
      : DEFAULT_FLOW_PROGRAM,
  version: null,
  config: null,
};

const REFINED_VALUE_DECORATION_OPTIONS = {
  inlineClassName: styles.refinedValueDecoration,
};

export default component TryFlow(
  defaultFlowVersion: string,
  flowVersions: $ReadOnlyArray<string>,
) {
  const {withBaseUrl} = useBaseUrlUtils();
  const [initialStateFromStorage, setInitialStateFromStorage] = useState(
    initialStateFromURI == null
      ? getHashedValue(localStorage.getItem(TRY_FLOW_LAST_CONTENT_STORAGE_KEY))
      : null,
  );
  const [flowVersion, setFlowVersion] = useState(
    initialState.version || defaultFlowVersion,
  );
  const editorRef = useRef(null);
  const previousDecorationsRef = useRef(null);
  const [errors, setErrors] = useState<$ReadOnlyArray<FlowJsError>>([]);
  const [internalError, setInternalError] = useState('');
  const [cursorPosition, setCursorPosition] =
    useState<?{lineNumber: number, column: number}>(null);
  const [ast, setAST] = useState<interface {} | string>({});
  const [loading, setLoading] = useState(true);
  const [flowService, setFlowService] = useState((null: ?FlowJsServices));
  const [activeToolbarTab, setActiveToolbarTab] = useState(
    ('code': 'code' | 'config'),
  );

  useEffect(() => {
    setLoading(true);
    FlowJsServices.init(withBaseUrl, flowVersion)
      .then(f => {
        setFlowService(existing =>
          existing == null
            ? // Only the initial init will use the config encoded in the starting URI
              f.withUpdatedConfig(initialState.config)
            : f,
        );
        setLoading(false);
        setInternalError('');
      })
      .catch(e => {
        setLoading(false);
        setInternalError(JSON.stringify(e));
      });
  }, [flowVersion]);

  useEffect(() => {
    forceRecheck();
  }, [flowService]);

  function resetFromStorage() {
    const model = monaco.editor.getModels()[0];
    if (initialStateFromStorage != null && model != null) {
      setInitialStateFromStorage(null);
      setFlowVersion(initialStateFromStorage.version || defaultFlowVersion);
      model.setValue(initialStateFromStorage.code);
      setFlowService(
        existing =>
          existing?.withUpdatedConfig(initialStateFromStorage.config) ??
          existing,
      );
    }
  }

  function semanticDecorations(flowService: FlowJsServices, model: any) {
    const decorations =
      flowService.semanticDecorations?.('-', model.getValue())?.decorations ||
      [];
    const refinedValueDecorations = [];
    for (const decoration of decorations) {
      switch (decoration.kind) {
        case 'refined-value':
          refinedValueDecorations.push({
            range: decoration.range,
            options: REFINED_VALUE_DECORATION_OPTIONS,
          });
          break;
      }
    }

    const editor = editorRef.current;
    if (editor == null) return;
    previousDecorationsRef.current?.clear();
    previousDecorationsRef.current = editor.createDecorationsCollection(
      refinedValueDecorations,
    );
  }

  function forceRecheck() {
    setAutoCompleteFunction(flowService);
    setGetDefFunction(flowService);
    setTypeAtPosFunction(flowService);
    setSignatureHelpFunction(flowService);

    const model = monaco.editor.getModels()[0];
    if (model == null || flowService == null) return;
    const value = model.getValue();

    // typecheck on edit
    try {
      const errors = flowService.checkContent('-', model.getValue());
      const markers = errors.map(err => {
        const messages = err.message;
        const firstLoc = messages[0].loc;
        const message = messages.map(msg => msg.descr).join('\n');
        let severity;
        switch (err.level) {
          case 'error':
            severity = monaco.MarkerSeverity.Error;
            break;
          case 'warning':
            severity = monaco.MarkerSeverity.Warning;
            break;
          default:
            severity = monaco.MarkerSeverity.Hint;
            break;
        }
        return {
          // the code is also in the message, so don't also include it here.
          // but if we fixed the message, we'd do this:
          // code: Array.isArray(err.error_codes) ? err.error_codes[0] : undefined,
          severity,
          message: message,
          source: firstLoc.source,
          startLineNumber: firstLoc.start.line,
          startColumn: firstLoc.start.column,
          endLineNumber: firstLoc.end.line,
          endColumn: firstLoc.end.column + 1,
          // TODO: show references
          // relatedInformation: ...
        };
      });
      monaco.editor.setModelMarkers(model, 'default', markers);
      semanticDecorations(flowService, model);
      setInternalError('');
      setErrors(errors);
      if (flowService?.supportsParse) {
        setAST(flowService.parseAstToJsonString(value));
      }
    } catch (e) {
      console.error(e);
      setInternalError(JSON.stringify(e));
      setErrors([]);
      setAST({});
    }

    // update the URL
    setHashedValue(flowService, flowVersion, value);

    // If we've made non-whitespace edits, remove notice to recover from storage.
    if (
      initialStateFromStorage != null &&
      value.replace(/\s/g, '') !== initialState.code.replace(/\s/g, '')
    ) {
      setInitialStateFromStorage(null);
    }
  }

  function onMount(editor: any) {
    forceRecheck();

    editor.onDidChangeCursorPosition(e => {
      setCursorPosition(e.position);
    });
    editorRef.current = editor;
  }

  return (
    <div className={styles.tryEditor}>
      <div className={styles.code}>
        <div className={styles.editorContainer}>
          <div className={styles.toolbar}>
            <ul className={styles.tabs}>
              <li
                className={clsx(
                  styles.tab,
                  activeToolbarTab === 'code' && styles.selectedTab,
                )}
                onClick={() => setActiveToolbarTab('code')}>
                Code
              </li>
              <li
                className={clsx(
                  styles.tab,
                  activeToolbarTab === 'config' && styles.selectedTab,
                )}
                onClick={() => setActiveToolbarTab('config')}>
                Config
              </li>
            </ul>
            {initialStateFromStorage && (
              <div className={styles.resetBanner}>
                <span>Recover from last saved state?</span>
                <button onClick={resetFromStorage}>Recover</button>
                <button onClick={() => setInitialStateFromStorage(null)}>
                  Ignore
                </button>
              </div>
            )}
          </div>
          <div
            style={{display: activeToolbarTab === 'config' ? 'block' : 'none'}}
            className={styles.tryEditorConfig}>
            <TryFlowConfigEditor
              flowService={flowService}
              setConfig={config =>
                setFlowService(flowService?.withUpdatedConfig(config))
              }
            />
          </div>
          <Editor
            defaultValue={initialState.code}
            defaultLanguage="flow"
            theme="vs-light"
            height="calc(100vh - var(--ifm-navbar-height) - 40px)"
            onChange={forceRecheck}
            onMount={onMount}
            options={{
              minimap: {enabled: false},
              hover: {enabled: true, above: false},
              scrollBeyondLastLine: false,
              overviewRulerBorder: false,
            }}
          />
        </div>
      </div>
      <TryFlowResults
        flowVersion={flowVersion}
        flowVersions={flowVersions}
        changeFlowVersion={event => setFlowVersion(event.target.value)}
        loading={loading}
        errors={errors}
        internalError={internalError}
        cursorPosition={
          cursorPosition != null
            ? {
                line: cursorPosition.lineNumber,
                column: cursorPosition.column - 1,
              }
            : null
        }
        ast={
          flowService?.supportsParse
            ? ast
            : 'AST output is not supported in this version of Flow.'
        }
      />
    </div>
  );
}
