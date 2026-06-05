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
import copyText from './clipboard';
import createTokensProvider from './tokens-theme-provider';
import flowLanguageConfiguration from './flow-configuration.json';

const TRY_FLOW_LAST_CONTENT_STORAGE_KEY = 'tryFlowLastContent';
const DEFAULT_FLOW_PROGRAM = `type User = {
  readonly name: string,
  readonly age: number,
};

function get<K extends keyof User>(user: User, key: K): User[K] {
  return user[key];
}

declare const user: User;
const age: number = get(user, 'name'); // Fix - update to: get(user, 'age')
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

component CodeIcon() {
  return (
    <svg
      viewBox="0 0 24 24"
      fill="none"
      stroke="currentColor"
      strokeWidth={2}
      strokeLinecap="round"
      strokeLinejoin="round"
      aria-hidden="true">
      <polyline points="16 18 22 12 16 6" />
      <polyline points="8 6 2 12 8 18" />
    </svg>
  );
}

component ConfigIcon() {
  return (
    <svg
      viewBox="0 0 24 24"
      fill="none"
      stroke="currentColor"
      strokeWidth={2}
      strokeLinecap="round"
      strokeLinejoin="round"
      aria-hidden="true">
      <line x1="21" x2="14" y1="4" y2="4" />
      <line x1="10" x2="3" y1="4" y2="4" />
      <line x1="21" x2="12" y1="12" y2="12" />
      <line x1="8" x2="3" y1="12" y2="12" />
      <line x1="21" x2="16" y1="20" y2="20" />
      <line x1="12" x2="3" y1="20" y2="20" />
      <line x1="14" x2="14" y1="2" y2="6" />
      <line x1="8" x2="8" y1="10" y2="14" />
      <line x1="16" x2="16" y1="18" y2="22" />
    </svg>
  );
}

component CopyLinkIcon() {
  return (
    <svg
      viewBox="0 0 24 24"
      fill="none"
      stroke="currentColor"
      strokeWidth={2}
      strokeLinecap="round"
      strokeLinejoin="round"
      aria-hidden="true">
      <path d="M10 13a5 5 0 0 0 7.54.54l3-3a5 5 0 0 0-7.07-7.07l-1.72 1.71" />
      <path d="M14 11a5 5 0 0 0-7.54-.54l-3 3a5 5 0 0 0 7.07 7.07l1.71-1.71" />
    </svg>
  );
}

component CopyIcon() {
  return (
    <svg
      viewBox="0 0 24 24"
      fill="none"
      stroke="currentColor"
      strokeWidth={2}
      strokeLinecap="round"
      strokeLinejoin="round"
      aria-hidden="true">
      <rect width="14" height="14" x="8" y="8" rx="2" ry="2" />
      <path d="M4 16c-1.1 0-2-.9-2-2V4c0-1.1.9-2 2-2h10c1.1 0 2 .9 2 2" />
    </svg>
  );
}

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
  const tryEditorRef = useRef<HTMLDivElement | null>(null);
  const [splitRatio, setSplitRatio] = useState(0.5);
  const previousDecorationsRef = useRef(null);
  const [errors, setErrors] = useState<$ReadOnlyArray<FlowJsError>>([]);
  const [internalError, setInternalError] = useState('');
  const [cursorPosition, setCursorPosition] =
    useState<?{lineNumber: number, column: number}>(null);
  const [ast, setAST] = useState<interface {} | string>({});
  const [loading, setLoading] = useState(true);
  const [flowService, setFlowService] = useState(null as ?FlowJsServices);
  const [activeToolbarTab, setActiveToolbarTab] = useState(
    'code' as 'code' | 'config',
  );
  const [copied, setCopied] = useState(null as ?('link' | 'code'));

  useEffect(() => {
    let cancelled = false;
    setLoading(true);
    FlowJsServices.init(withBaseUrl, flowVersion)
      .then(f => {
        if (cancelled) {
          return;
        }
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
        if (cancelled) {
          return;
        }
        setLoading(false);
        setInternalError(JSON.stringify(e));
      });
    return () => {
      cancelled = true;
    };
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

  function copyToClipboard(kind: 'link' | 'code', text: string) {
    // Only show the "Copied!" confirmation if the write actually succeeds.
    // copyText falls back to execCommand where the async Clipboard API is
    // unavailable or blocked (insecure context, unfocused doc, embedded iframe).
    copyText(text).then(ok => {
      if (!ok) return;
      setCopied(kind);
      setTimeout(() => setCopied(null), 1500);
    });
  }

  function onSplitterDown(e: SyntheticMouseEvent<>) {
    e.preventDefault();
    const container = tryEditorRef.current;
    if (container == null) return;
    const body = document.body;
    const onMove = (ev: MouseEvent) => {
      const rect = container.getBoundingClientRect();
      const ratio = (ev.clientX - rect.left) / rect.width;
      setSplitRatio(Math.min(0.8, Math.max(0.2, ratio)));
    };
    const onUp = () => {
      window.removeEventListener('mousemove', onMove);
      window.removeEventListener('mouseup', onUp);
      if (body != null) {
        body.style.cursor = '';
        body.style.userSelect = '';
      }
    };
    window.addEventListener('mousemove', onMove);
    window.addEventListener('mouseup', onUp);
    if (body != null) {
      body.style.cursor = 'col-resize';
      body.style.userSelect = 'none';
    }
  }

  // Keyboard control for the splitter: arrows nudge the ratio, Home/End jump to
  // the min/max, matching the 0.2–0.8 clamp used by the mouse drag.
  function onSplitterKey(e: SyntheticKeyboardEvent<>) {
    const step = 0.02;
    let next;
    switch (e.key) {
      case 'ArrowLeft':
        next = splitRatio - step;
        break;
      case 'ArrowRight':
        next = splitRatio + step;
        break;
      case 'Home':
        next = 0.2;
        break;
      case 'End':
        next = 0.8;
        break;
      default:
        return;
    }
    e.preventDefault();
    setSplitRatio(Math.min(0.8, Math.max(0.2, next)));
  }

  function revealError(line: number, column: number) {
    const editor = editorRef.current;
    if (editor == null) return;
    editor.revealLineInCenter(line);
    editor.setPosition({lineNumber: line, column});
    editor.focus();
  }

  function semanticDecorations(flowService: FlowJsServices, model: any) {
    let decorations: $ReadOnlyArray<{kind: 'refined-value', range: any}> = [];
    try {
      decorations =
        flowService.semanticDecorations?.('-', model.getValue())?.decorations ||
        [];
    } catch {
      // Refined value decorations are best-effort UI hints. They should not
      // interrupt typechecking or surface as Try Flow internal errors.
    }
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
    <div
      className={styles.tryEditor}
      ref={tryEditorRef}
      style={{'--tf-split': splitRatio} as $FlowFixMe}>
      <div className={styles.code}>
        <div className={styles.editorContainer}>
          <div className={styles.toolbar}>
            <div className={styles.tabs}>
              <button
                type="button"
                className={clsx(
                  styles.tab,
                  activeToolbarTab === 'code' && styles.selectedTab,
                )}
                aria-pressed={activeToolbarTab === 'code'}
                onClick={() => setActiveToolbarTab('code')}>
                <CodeIcon />
                Code
              </button>
              <button
                type="button"
                className={clsx(
                  styles.tab,
                  activeToolbarTab === 'config' && styles.selectedTab,
                )}
                aria-pressed={activeToolbarTab === 'config'}
                onClick={() => setActiveToolbarTab('config')}>
                <ConfigIcon />
                Config
              </button>
            </div>
            <div className={styles.toolbarActions}>
              {initialStateFromStorage && (
                <div className={styles.resetBanner}>
                  <span>Recover from last saved state?</span>
                  <button
                    className={styles.toolbarButton}
                    onClick={resetFromStorage}>
                    Recover
                  </button>
                  <button
                    className={styles.toolbarButton}
                    onClick={() => setInitialStateFromStorage(null)}>
                    Ignore
                  </button>
                </div>
              )}
              <button
                className={styles.toolbarButton}
                onClick={() =>
                  copyToClipboard(
                    'code',
                    monaco.editor.getModels()[0]?.getValue() ?? '',
                  )
                }
                title="Copy the code">
                <CopyIcon />
                {copied === 'code' ? 'Copied!' : 'Copy code'}
              </button>
              <button
                className={styles.toolbarButton}
                onClick={() => copyToClipboard('link', window.location.href)}
                title="Copy a shareable link to this code">
                <CopyLinkIcon />
                {copied === 'link' ? 'Copied!' : 'Copy link'}
              </button>
            </div>
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
            height="var(--tf-editor-h)"
            onChange={forceRecheck}
            onMount={onMount}
            options={{
              minimap: {enabled: false},
              hover: {enabled: true, above: false},
              scrollBeyondLastLine: false,
              overviewRulerBorder: false,
              // Match the site's code font (Monaco uses its own setting, not the
              // CSS --ifm-font-family-monospace var).
              fontFamily:
                "'Geist Mono Variable', ui-monospace, SFMono-Regular, Menlo, Consolas, monospace",
              // Breathing room between the tab bar and the first line of code.
              padding: {top: 12},
            }}
          />
        </div>
      </div>
      <div
        className={styles.splitter}
        role="separator"
        tabIndex={0}
        aria-label="Resize editor and results panes"
        aria-orientation="vertical"
        aria-valuemin={20}
        aria-valuemax={80}
        aria-valuenow={Math.round(splitRatio * 100)}
        onMouseDown={onSplitterDown}
        onKeyDown={onSplitterKey}
      />
      <TryFlowResults
        flowVersion={flowVersion}
        flowVersions={flowVersions}
        changeFlowVersion={event => setFlowVersion(event.target.value)}
        loading={loading}
        errors={errors}
        internalError={internalError}
        onErrorSelect={revealError}
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
