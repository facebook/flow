/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

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

export function load(version) {
  if (version in versionCache) {
    return Promise.resolve(versionCache[version]);
  }
  const majorVersion =
    version === 'master'
    ? Infinity
    : parseInt(version.split('.')[1], 10);
  const libs = majorVersion <= 54 ? [
    `/static/${version}/flowlib/core.js`,
    `/static/${version}/flowlib/bom.js`,
    `/static/${version}/flowlib/cssom.js`,
    `/static/${version}/flowlib/dom.js`,
    `/static/${version}/flowlib/node.js`,
    `/static/${version}/flowlib/react.js`,
    `/static/${version}/flowlib/streams.js`,
  ] : majorVersion <= 71 ? [
    `/static/${version}/flowlib/core.js`,
    `/static/${version}/flowlib/react.js`,
  ] : [
    `/static/${version}/flowlib/core.js`,
    `/static/${version}/flowlib/react.js`,
    `/static/${version}/flowlib/intl.js`,
  ];
  const flowLoader = new Promise(function(resolve) {
    requirejs([`/static/${version}/flow.js`], resolve);
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
      return flow;
    })
    .catch(function(err) {
      throw err;
    });
}
