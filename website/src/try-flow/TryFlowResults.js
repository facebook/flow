/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import type {CollapsedFieldProps} from 'react-json-view';

import * as React from 'react';
import {useState, type MixedElement} from 'react';
import ReactJson from 'react-json-view';
import clsx from 'clsx';
import styles from './TryFlow.module.css';

component ErrorMessage(msg: FlowJsErrorMessage) {
  if (msg.loc && msg.context != null) {
    const basename = msg.loc.source.replace(/.*\//, '');
    const filename = basename !== '-' ? `${msg.loc.source}:` : '';
    const prefix = `${filename}${msg.loc.start.line}: `;

    const before = msg.context.slice(0, msg.loc.start.column - 1);
    const highlight =
      msg.loc.start.line === msg.loc.end.line
        ? msg.context.slice(msg.loc.start.column - 1, msg.loc.end.column)
        : msg.context.slice(msg.loc.start.column - 1);
    const after =
      msg.loc.start.line === msg.loc.end.line
        ? msg.context.slice(msg.loc.end.column)
        : '';

    const offset = msg.loc.start.column + prefix.length - 1;
    const arrow = `${(prefix + before).replace(/[^ ]/g, ' ')}^ `;

    return (
      <>
        <div>
          {prefix + before}
          <strong className={styles.msgHighlight}>{highlight}</strong>
          {after}
        </div>
        {arrow}
        <span className={styles.msgType}>{msg.descr}</span>
      </>
    );
  } else if (msg.type === 'Comment') {
    return `. ${msg.descr}\n`;
  } else {
    return `${msg.descr}\n`;
  }
}

component ErrorMessageExtra(info: FlowJsErrorMessageInformation) {
  return (
    <ul>
      <li>
        {info.message &&
          info.message.map((msg, i) => <ErrorMessage key={i} msg={msg} />)}
      </li>
      <li>
        {info.children &&
          info.children.map((info, i) => (
            <ErrorMessageExtra key={i} info={info} />
          ))}
      </li>
    </ul>
  );
}

type Position = {line: number, column: number};

function comparePosition(a: Position, b: Position): number {
  if (a.line > b.line) {
    return 1;
  } else if (a.line < b.line) {
    return -1;
  } else if (a.column > b.column) {
    return 1;
  } else if (a.column < b.column) {
    return -1;
  } else {
    return 0;
  }
}

function shouldCollapse(cursorPosition: Position, json: mixed): boolean {
  if (
    typeof json === 'object' &&
    json != null &&
    typeof json.loc === 'object'
  ) {
    const loc: {start: Position, end: Position} = (json.loc: any);
    if (
      comparePosition(loc.start, cursorPosition) <= 0 &&
      comparePosition(cursorPosition, loc.end) <= 0
    ) {
      return false;
    }
  }
  if (Array.isArray(json)) {
    return json.every(child => shouldCollapse(cursorPosition, child));
  }
  return true;
}

export default component TryFlowResults(
  flowVersion: string,
  flowVersions: $ReadOnlyArray<string>,
  changeFlowVersion: (SyntheticInputEvent<>) => void,
  loading: boolean,
  errors: $ReadOnlyArray<FlowJsError>,
  internalError: string,
  cursorPosition: ?Position,
  ast: interface {} | string,
) {
  const [activeToolbarTab, setActiveToolbarTab] = useState('errors');

  return (
    <div className={styles.results}>
      <div className={styles.toolbar}>
        <ul className={styles.tabs}>
          <li
            className={clsx(
              styles.tab,
              activeToolbarTab === 'errors' && styles.selectedTab,
            )}
            onClick={() => setActiveToolbarTab('errors')}>
            Errors
          </li>
          <li
            className={clsx(
              styles.tab,
              activeToolbarTab === 'json' && styles.selectedTab,
            )}
            onClick={() => setActiveToolbarTab('json')}>
            JSON
          </li>
          <li
            className={clsx(
              styles.tab,
              activeToolbarTab === 'ast' && styles.selectedTab,
            )}
            onClick={() => setActiveToolbarTab('ast')}>
            AST
          </li>
        </ul>
        <div className={styles.version}>
          {flowVersion !== flowVersions[0] &&
          flowVersion !== flowVersions[1] ? (
            <span className={styles.versionWarning}>old version selected</span>
          ) : null}
          <select value={flowVersion} onChange={changeFlowVersion}>
            {flowVersions.map(version => (
              <option key={version} value={version}>
                {version}
              </option>
            ))}
          </select>
        </div>
      </div>
      {loading && (
        <div>
          <div className={styles.loader}>
            <div className={styles.bounce1}></div>
            <div className={styles.bounce2}></div>
            <div></div>
          </div>
        </div>
      )}
      {!loading && activeToolbarTab === 'errors' && (
        <pre className={clsx(styles.resultBody, styles.errors)}>
          <ul>
            {internalError ? (
              <li>TryFlow encountered an internal error: {internalError}</li>
            ) : errors.length === 0 ? (
              <li>No errors!</li>
            ) : (
              errors.map((error, i) => (
                <li key={i}>
                  {error.message.map((msg, i) => (
                    <ErrorMessage key={i} msg={msg} />
                  ))}
                  {error.extra &&
                    error.extra.map((info, i) => (
                      <ErrorMessageExtra key={i} info={info} />
                    ))}
                </li>
              ))
            )}
          </ul>
        </pre>
      )}
      {!loading && activeToolbarTab === 'json' && (
        <pre className={styles.resultBody}>
          {JSON.stringify(errors, null, 2)}
        </pre>
      )}
      {!loading &&
        activeToolbarTab === 'ast' &&
        (typeof ast === 'string' ? (
          <pre className={styles.resultBody}>{ast}</pre>
        ) : (
          <ReactJson
            src={ast}
            indentWidth={2}
            quotesOnKeys={false}
            displayDataTypes={false}
            enableClipboard={false}
            shouldCollapse={field => {
              if (cursorPosition == null) {
                return false;
              }
              return shouldCollapse(cursorPosition, field.src);
            }}
          />
        ))}
    </div>
  );
}
