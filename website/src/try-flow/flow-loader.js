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
      ? [
          `/flow/master/flowlib/core.js`,
          `/flow/master/flowlib/react.js`,
          `/flow/master/flowlib/intl.js`,
        ].map(withBaseUrl)
      : majorVersion <= 54
      ? [
          `${versionedUnpkgComUrl(version)}/flowlib/core.js`,
          `${versionedUnpkgComUrl(version)}/flowlib/bom.js`,
          `${versionedUnpkgComUrl(version)}/flowlib/cssom.js`,
          `${versionedUnpkgComUrl(version)}/flowlib/dom.js`,
          `${versionedUnpkgComUrl(version)}/flowlib/node.js`,
          `${versionedUnpkgComUrl(version)}/flowlib/react.js`,
          `${versionedUnpkgComUrl(version)}/flowlib/streams.js`,
        ]
      : majorVersion <= 71
      ? [
          `${versionedUnpkgComUrl(version)}/flowlib/core.js`,
          `${versionedUnpkgComUrl(version)}/flowlib/react.js`,
        ]
      : [
          `${versionedUnpkgComUrl(version)}/flowlib/core.js`,
          `${versionedUnpkgComUrl(version)}/flowlib/react.js`,
          `${versionedUnpkgComUrl(version)}/flowlib/intl.js`,
        ];
  const flowLoader = new Promise<[string, string]>(resolve => {
    requirejs(
      [
        version === 'master'
          ? withBaseUrl('/flow/master/flow.js')
          : `${versionedUnpkgComUrl(version)}/flow.js`,
        ,
      ],
      resolve,
    );
  });
  return Promise.all([flowLoader, ...libs.map(get)]).then(
    ([_flow, ...contents]) => {
      contents.forEach(nameAndContent => {
        self.flow.registerFile(nameAndContent[0], nameAndContent[1]);
      });
      self.flow.registerFile('try-lib.js', TRY_LIB_CONTENTS);
      if (majorVersion <= 126) {
        self.flow.setLibs([...libs.map(normalizeUrlForFilename), 'try-lib.js']);
      } else {
        self.flow.initBuiltins([
          ...libs.map(normalizeUrlForFilename),
          'try-lib.js',
        ]);
      }
      versionCache.set(version, self.flow);
      // $FlowFixMe[cannot-resolve-name]
      return flow;
    },
  );
}
