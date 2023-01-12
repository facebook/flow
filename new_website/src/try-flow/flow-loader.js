/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// defines window.requirejs
import './require_2_3_3';

declare function requirejs(pathList: $ReadOnlyArray<string>, resolve: (any) => void): void;

const versionCache = {};

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

function get(url) {
  return new Promise(function(resolve, reject) {
    var req = new XMLHttpRequest();
    req.open('GET', url);
    req.onload = function() {
      if (req.status == 200) {
        resolve([url, req.response]);
      }
      else {
        reject(Error(req.statusText));
      }
    };
    req.onerror = function() {
      reject(Error("Network Error"));
    };
    req.send();
  });
}

export function load(version: string): Promise<FlowJs> {
  if (version in versionCache) {
    return Promise.resolve(versionCache[version]);
  }
  const majorVersion =
    version === 'master'
    ? Infinity
    : parseInt(version.split('.')[1], 10);
  const libs = majorVersion <= 54 ? [
    `/flow/${version}/flowlib/core.js`,
    `/flow/${version}/flowlib/bom.js`,
    `/flow/${version}/flowlib/cssom.js`,
    `/flow/${version}/flowlib/dom.js`,
    `/flow/${version}/flowlib/node.js`,
    `/flow/${version}/flowlib/react.js`,
    `/flow/${version}/flowlib/streams.js`,
  ] : majorVersion <= 71 ? [
    `/flow/${version}/flowlib/core.js`,
    `/flow/${version}/flowlib/react.js`,
  ] : [
    `/flow/${version}/flowlib/core.js`,
    `/flow/${version}/flowlib/react.js`,
    `/flow/${version}/flowlib/intl.js`,
  ];
  const flowLoader = new Promise(function(resolve) {
    requirejs([`/flow/${version}/flow.js`], resolve);
  });
  return Promise.all([flowLoader, ...libs.map(get)])
    .then(function([_flow, ...contents]) {
      contents.forEach(function(nameAndContent) {
        self.flow.registerFile(nameAndContent[0], nameAndContent[1]);
      });
      self.flow.registerFile('try-lib.js', TRY_LIB_CONTENTS);
      if (majorVersion <= 126) {
        self.flow.setLibs([...libs, 'try-lib.js']);
      } else {
        self.flow.initBuiltins([...libs, 'try-lib.js']);
      }
      versionCache[version] = self.flow;
      // $FlowFixMe[cannot-resolve-name]
      return flow;
    })
    .catch(function(err) {
      throw err;
    });
}
