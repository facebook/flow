/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import React, {useState, useEffect, useRef} from 'react';
import clsx from 'clsx';
import styles from './TryFlow.module.css';
import TryFlowEditor from './TryFlowEditor';
import initFlow from './init-flow';

function getASTJSON(flow, value) {
  const options = {
    esproposal_class_instance_fields: true,
    esproposal_class_static_fields: true,
    esproposal_decorators: true,
    esproposal_export_star_as: true,
    esproposal_optional_chaining: true,
    esproposal_nullish_coalescing: true,
    types: true,
  };
  return flow.parse(value, options).then(ast => JSON.stringify(ast, null, 2));
}

function ErrorMessage({msg}) {
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

function ErrorMessageExtra({info}) {
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

export default function TryFlowResults({
  flowVersion,
  flowVersions,
  changeFlowVersion,
  loading,
  errors,
  ast,
}) {
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
