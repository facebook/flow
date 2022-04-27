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
import * as LZString from 'lz-string';
import styles from './TryFlow.module.css';
import TryFlowEditor, {monaco} from './TryFlowEditor';
import TryFlowResults from './TryFlowResults';
import initFlow from './init-flow';
import type {AsyncFlow} from './init-flow';

function getASTJSON(flow: AsyncFlow, value: string) {
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

function asSeverity(severity: string) {
  switch (severity) {
    case 'error':
      return monaco.MarkerSeverity.Error;
    case 'warning':
      return monaco.MarkerSeverity.Warning;
    default:
      return monaco.MarkerSeverity.Hint;
  }
}

function validateFlowCode(
  flow: Promise<AsyncFlow>,
  model,
  callback: ($ReadOnlyArray<FlowJsError>, string) => void,
) {
  Promise.resolve(flow)
    .then(flowProxy => flowProxy.checkContent('-', model.getValue()))
    .then(errors => {
      const markers = errors.map(err => {
        const messages = err.message;
        const firstLoc = messages[0].loc;
        const message = messages.map(msg => msg.descr).join('\n');
        return {
          // the code is also in the message, so don't also include it here.
          // but if we fixed the message, we'd do this:
          // code: Array.isArray(err.error_codes) ? err.error_codes[0] : undefined,
          severity: asSeverity(err.level),
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
      if (callback != null) {
        callback(errors, model.getValue());
      }
    });
}

type Props = {defaultFlowVersion: string, flowVersions: $ReadOnlyArray<string>};

export default function TryFlow({
  defaultFlowVersion,
  flowVersions,
}: Props): MixedElement {
  const [flowVersion, setFlowVersion] = useState(defaultFlowVersion);
  const [loading, setLoading] = useState(true);
  const [supportsParse, setSupportParse] = useState(false);
  const [errors, setErrors] = useState([]);
  const [astJSON, setASTJSON] = useState('{}');

  const flowRef = useRef(initFlow(flowVersion));
  const editorRef = useRef(null);
  flowRef.current.then(() => setLoading(false));

  useEffect(() => {
    flowRef.current?.then(flow => flow.supportsParse().then(setSupportParse));
  }, [flowVersion]);

  function changeFlowVersion(event) {
    const version = event.target.value;
    setLoading(true);
    setFlowVersion(version);
    flowRef.current = initFlow(version);
    flowRef.current.then(() => setLoading(false));
  }

  function onFlowErrors(errors, value) {
    setErrors(errors);
    flowRef.current.then(flow => {
      if (supportsParse) {
        getASTJSON(flow, value).then(setASTJSON);
      }
    });
  }

  function onCodeChange() {
    const model = monaco.editor.getModels()[0];
    if (model == null) return;
    const value = model.getValue();
    const flow = flowRef.current;

    // typecheck on edit
    validateFlowCode(flow, model, onFlowErrors);
    // update the URL
    const encoded = LZString.compressToEncodedURIComponent(value);
    window.location.hash = `0${encoded}`;
    localStorage.setItem('tryFlowLastContent', location.hash);
  }

  return (
    <div className={styles.tryEditor}>
      <div className={styles.code}>
        <TryFlowEditor
          key={flowVersion}
          editorRef={editorRef}
          flowRef={flowRef}
          onCodeChange={onCodeChange}
        />
      </div>
      <TryFlowResults
        flowVersion={flowVersion}
        flowVersions={flowVersions}
        changeFlowVersion={changeFlowVersion}
        loading={loading}
        errors={errors}
        ast={
          supportsParse
            ? astJSON
            : 'AST output is not supported in this version of Flow.'
        }
      />
    </div>
  );
}
