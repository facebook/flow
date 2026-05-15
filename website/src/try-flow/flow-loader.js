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

const MASTER_VERSION = 'master';
const MASTER_RUST_PORT_VERSION = 'master (rust port)';
const FLOW_RELEASE_VERSION =
  /^\d+\.\d+\.\d+(?:-[0-9A-Za-z]+(?:[.-][0-9A-Za-z]+)*)?$/;

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

function normalizeFlowReleaseVersion(version: string): string {
  const normalizedVersion = version.startsWith('v')
    ? version.substring(1)
    : version;
  if (!FLOW_RELEASE_VERSION.test(normalizedVersion)) {
    throw new Error(`Invalid Flow release version: ${version}`);
  }
  return normalizedVersion;
}

function versionedUnpkgComUrl(version: string): string {
  const releaseVersion = normalizeFlowReleaseVersion(version);
  return `https://unpkg.com/try-flow-website-js@${encodeURIComponent(
    releaseVersion,
  )}`;
}

function masterVersionBasePath(version: string): ?string {
  switch (version) {
    case MASTER_VERSION:
      return '/flow/master';
    case MASTER_RUST_PORT_VERSION:
      return '/flow/master-rust-port';
    default:
      return null;
  }
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
  const masterBasePath = masterVersionBasePath(version);
  let flowPath: string;
  let libs: Array<string>;
  if (masterBasePath != null) {
    flowPath = withBaseUrl(`${masterBasePath}/flow.js`);
    libs = [
      `${masterBasePath}/flowlib/core.js`,
      `${masterBasePath}/flowlib/react.js`,
    ].map(withBaseUrl);
  } else {
    let releaseVersion;
    try {
      releaseVersion = normalizeFlowReleaseVersion(version);
    } catch (error) {
      return Promise.reject(error);
    }

    const versionedBaseUrl = versionedUnpkgComUrl(releaseVersion);
    const minorVersion = parseInt(releaseVersion.split('.')[1], 10);
    flowPath = `${versionedBaseUrl}/flow.js`;
    libs =
      minorVersion >= 266
        ? [
            `${versionedBaseUrl}/flowlib/core.js`,
            `${versionedBaseUrl}/flowlib/react.js`,
          ]
        : [
            `${versionedBaseUrl}/flowlib/core.js`,
            `${versionedBaseUrl}/flowlib/react.js`,
            `${versionedBaseUrl}/flowlib/intl.js`,
          ];
  }
  const flowLoader = new Promise<FlowJs>((resolve, reject) => {
    requirejs([flowPath], flowModule => {
      try {
        resolve(getLoadedFlow(flowModule));
      } catch (error) {
        reject(error);
      }
    });
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
