/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

// defines window.requirejs
// $FlowFixMe[cannot-resolve-module]
import './require_2_3_3';

declare function requirejs(
  pathList: $ReadOnlyArray<string>,
  resolve: (any) => void,
): void;

const versionCache /*: Map<string, Promise<FlowJs>> */ = new Map();

const TRY_LIB_CONTENTS = `
declare type $JSXIntrinsics = {
  [string]: {
    instance: any,
    props: {
      children?: React$Node,
      [key: string]: any,
    },
  },
};
`.slice(1);

// We need to normalize the URL.
// The register file function only accepts absolute path.
function normalizeUrlForFilename(url: string) {
  return url.startsWith('https://unpkg.com/')
    ? url.substring('https://unpkg.com'.length)
    : url;
}

function get(url: string) {
  return new Promise<[string, string]>((resolve, reject) => {
    const req = new XMLHttpRequest();
    req.timeout = 5000;
    req.onload = () => {
      if (req.status === 200) {
        const name = normalizeUrlForFilename(url);
        resolve([name, req.response]);
      } else {
        reject(Error(req.statusText));
      }
    };
    req.onerror = () => {
      reject('Network error');
    };
    req.ontimeout = () => {
      reject('Network timed out');
    };
    req.onabort = () => {
      reject('Network request aborted');
    };
    req.open('GET', url);
    req.send();
  });
}

function versionedUnpkgComUrl(version: string): string {
  version = version.startsWith('v') ? version.substring(1) : version;
  return `https://unpkg.com/try-flow-website-js@${version}`;
}

function isFlowJs(value: mixed): boolean {
  return (
    value != null &&
    typeof (value as any).checkContent === 'function' &&
    typeof (value as any).registerFile === 'function' &&
    typeof (value as any).initBuiltins === 'function'
  );
}

function getGlobalFlow(): ?FlowJs {
  const globalObject =
    typeof self !== 'undefined'
      ? self
      : typeof window !== 'undefined'
        ? window
        : typeof globalThis !== 'undefined'
          ? globalThis
          : null;
  if (globalObject == null || !isFlowJs((globalObject as any).flow)) {
    return null;
  }
  return (globalObject as any).flow;
}

function getLoadedFlow(flowModule: mixed): FlowJs {
  if (isFlowJs(flowModule)) {
    return flowModule as any;
  }
  const globalFlow = getGlobalFlow();
  if (globalFlow != null) {
    return globalFlow;
  }
  if (flowModule != null && isFlowJs((flowModule as any).default)) {
    return (flowModule as any).default;
  }
  throw new Error('flow.js loaded without exposing a Flow API');
}

function waitForReady(flow: FlowJs): Promise<FlowJs> {
  const ready = flow.ready;
  if (ready == null) {
    return Promise.resolve(flow);
  }
  return Promise.resolve(ready).then(() => flow);
}

export function load(
  withBaseUrl: string => string,
  version: string,
): Promise<FlowJs> {
  const cached = versionCache.get(version);
  if (cached) {
    return Promise.resolve(cached);
  }
  const majorVersion =
    version === 'master' ? Infinity : parseInt(version.split('.')[1], 10);
  const libs =
    version === 'master'
      ? [`/flow/master/flowlib/core.js`, `/flow/master/flowlib/react.js`].map(
          withBaseUrl,
        )
      : majorVersion >= 266
        ? [
            `${versionedUnpkgComUrl(version)}/flowlib/core.js`,
            `${versionedUnpkgComUrl(version)}/flowlib/react.js`,
          ]
        : [
            `${versionedUnpkgComUrl(version)}/flowlib/core.js`,
            `${versionedUnpkgComUrl(version)}/flowlib/react.js`,
            `${versionedUnpkgComUrl(version)}/flowlib/intl.js`,
          ];
  const flowLoader = new Promise<FlowJs>((resolve, reject) => {
    requirejs(
      [
        version === 'master'
          ? withBaseUrl('/flow/master/flow.js')
          : `${versionedUnpkgComUrl(version)}/flow.js`,
      ],
      flowModule => {
        try {
          resolve(getLoadedFlow(flowModule));
        } catch (error) {
          reject(error);
        }
      },
    );
  });
  return Promise.all([
    flowLoader.then(waitForReady),
    ...libs.map(get),
  ] as any).then(([flow, ...contents]) => {
    contents.forEach(nameAndContent => {
      flow.registerFile(nameAndContent[0], nameAndContent[1]);
    });
    flow.registerFile('try-lib.js', TRY_LIB_CONTENTS);
    flow.initBuiltins([...libs.map(normalizeUrlForFilename), 'try-lib.js']);
    versionCache.set(version, flow);
    return flow;
  });
}
