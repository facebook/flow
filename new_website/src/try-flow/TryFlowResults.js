/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import React, {useState, type MixedElement} from 'react';
import clsx from 'clsx';
import styles from './TryFlow.module.css';
import TryFlowEditor from './TryFlowEditor';
import initFlow from './init-flow';

function ErrorMessage({msg}: {msg: FlowJsErrorMessage}) {
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

function ErrorMessageExtra({info}: {info: FlowJsErrorMessageInformation}) {
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

type Props = {
  flowVersion: string,
  flowVersions: $ReadOnlyArray<string>,
  changeFlowVersion: (SyntheticInputEvent<>) => void,
  loading: boolean,
  errors: $ReadOnlyArray<FlowJsError>,
  ast: string,
};

export default function TryFlowResults({
  flowVersion,
  flowVersions,
  changeFlowVersion,
  loading,
  errors,
  ast,
}: Props): MixedElement {
  const [activeToolbarTab, setActiveToolbarTab] = useState('errors');

  return (
    <div className={styles.results}>
      <ul className={styles.toolbar}>
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
        <li className={styles.version}>
          <select value={flowVersion} onChange={changeFlowVersion}>
            {flowVersions.map(version => (
              <option key={version} value={version}>
                {version}
              </option>
            ))}
          </select>
        </li>
      </ul>
      {activeToolbarTab === 'errors' && (
        <pre className={clsx(styles.resultBody, styles.errors)}>
          <ul>
            {errors.map((error, i) => (
              <li key={i}>
                {error.message.map((msg, i) => (
                  <ErrorMessage key={i} msg={msg} />
                ))}
                {error.extra &&
                  error.extra.map((info, i) => (
                    <ErrorMessageExtra key={i} info={info} />
                  ))}
              </li>
            ))}
          </ul>
        </pre>
      )}
      {activeToolbarTab === 'json' && (
        <pre className={styles.resultBody}>
          {JSON.stringify(errors, null, 2)}
        </pre>
      )}
      {activeToolbarTab === 'ast' && (
        <pre className={styles.resultBody}>{ast}</pre>
      )}

      {loading && (
        <div>
          <div className={styles.loader}>
            <div className={styles.bounce1}></div>
            <div className={styles.bounce2}></div>
            <div></div>
          </div>
        </div>
      )}
    </div>
  );
}
