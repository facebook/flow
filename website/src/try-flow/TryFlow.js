/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import React, {useState, useEffect, useRef, type MixedElement} from 'react';
import clsx from 'clsx';
import Editor from '@monaco-editor/react';
import * as LZString from 'lz-string';
import styles from './TryFlow.module.css';
import TryFlowConfigEditor from './TryFlowConfigEditor';
import TryFlowResults from './TryFlowResults';
import {monaco, setTypeAtPosFunction} from './configured-monaco';
import FlowJsServices from './flow-services';
import createTokensProvider from './tokens-theme-provider';
import flowLanguageConfiguration from './flow-configuration.json';

const TRY_FLOW_LAST_CONTENT_STORAGE_KEY = 'tryFlowLastContent';
const DEFAULT_FLOW_PROGRAM = `/* @flow */

function foo(x: ?number): string {
  if (x) {
    return x;
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
  code: DEFAULT_FLOW_PROGRAM,
  version: null,
  config: null,
};

export default function TryFlow({
  defaultFlowVersion,
  flowVersions,
}: {
  defaultFlowVersion: string,
  flowVersions: $ReadOnlyArray<string>,
}): MixedElement {
  const [initialStateFromStorage, setInitialStateFromStorage] = useState(
    initialStateFromURI == null
      ? getHashedValue(localStorage.getItem(TRY_FLOW_LAST_CONTENT_STORAGE_KEY))
      : null,
  );
  const [flowVersion, setFlowVersion] = useState(
    initialState.version || defaultFlowVersion,
  );
  const [errors, setErrors] = useState([]);
  const [internalError, setInternalError] = useState('');
  const [astJSON, setASTJSON] = useState('{}');
  const [loading, setLoading] = useState(true);
  const [flowService, setFlowService] = useState((null: ?FlowJsServices));
  const [activeToolbarTab, setActiveToolbarTab] = useState(
    ('code': 'code' | 'config'),
  );

  useEffect(() => {
    setLoading(true);
    FlowJsServices.init(flowVersion).then(f => {
      setFlowService(existing =>
        existing == null
          ? // Only the initial init will use the config encoded in the starting URI
            f.withUpdatedConfig(initialState.config)
          : f,
      );
      setLoading(false);
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

  function forceRecheck() {
    setTypeAtPosFunction(flowService);

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
      setInternalError('');
      setErrors(errors);
      if (flowService?.supportsParse) {
        setASTJSON(flowService.parseAstToJsonString(value));
      }
    } catch (e) {
      console.error(e);
      setInternalError(JSON.stringify(e));
      setErrors([]);
      setASTJSON('{}');
    }

    // update the URL
    setHashedValue(flowService, flowVersion, value);
  }

  return (
    <div className={styles.tryEditor}>
      <div className={styles.code}>
        <div className={styles.editorContainer}>
          <ul className={styles.toolbar}>
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
              <span>Do you want to recover from the last saved state?</span>
              <div>
                <button
                  style={{marginRight: '0.5rem'}}
                  onClick={resetFromStorage}>
                  Recover
                </button>
                <button onClick={() => setInitialStateFromStorage(null)}>
                  Ignore
                </button>
              </div>
            </div>
          )}
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
            onMount={forceRecheck}
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
        ast={
          flowService?.supportsParse
            ? astJSON
            : 'AST output is not supported in this version of Flow.'
        }
      />
    </div>
  );
}
