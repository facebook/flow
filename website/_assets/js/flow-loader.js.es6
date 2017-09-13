//
// Copyright (c) 2013-present, Facebook, Inc.
// All rights reserved.
//
// This source code is licensed under the BSD-style license found in the
// LICENSE file in the "flow" directory of this source tree. An additional grant
// of patent rights can be found in the PATENTS file in the same directory.
//

const versionCache = {};

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
  const libs = [
    `/static/${version}/flowlib/core.js`,
    // `/static/${version}/flowlib/bom.js`,
    // `/static/${version}/flowlib/cssom.js`,
    // `/static/${version}/flowlib/dom.js`,
    // `/static/${version}/flowlib/node.js`,
    `/static/${version}/flowlib/react.js`,
    // `/static/${version}/flowlib/react-dom.js`,
    // `/static/${version}/flowlib/streams.js`,
  ];
  const flowLoader = new Promise(function(resolve) {
    require([`/static/${version}/flow.js`], resolve);
  });
  return Promise.all([flowLoader, ...libs.map(get)])
    .then(function([_flow, ...contents]) {
      contents.forEach(function(nameAndContent) {
        self.flow.registerFile(nameAndContent[0], nameAndContent[1]);
      });
      self.flow.setLibs(libs);
      versionCache[version] = self.flow;
      return flow;
    })
    .catch(function(err) {
      throw err;
    });
}
