/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import {load} from './js/flow-loader';

var versionCache = {};

this.onmessage = function(e) {
  var data = e.data;
  switch (data.type) {
    // preload flow. optional, but makes sure flow is ready
    case "init":
      getFlow(data.version).then(function() {
        postMessage({id: data.id, type: "init", result: true});
      })["catch"](function (e) {
        postMessage({id: data.id, type: "init", err: e});
      });
      return;
    case "checkContent":
      getFlow(data.version).then(function(flow) {
        var result = flow.checkContent(data.filename, data.body);
        postMessage({id: data.id, type: "checkContent", result: result });
      })["catch"](function (e) {
        postMessage({id: data.id, type: "checkContent", err: e});
      });
      return;
    case "typeAtPos":
      getFlow(data.version).then(function(flow) {
        var result = flow.typeAtPos(
          data.filename,
          data.body,
          data.line,
          data.col
        );
        postMessage({id: data.id, type: "typeAtPos", result: result});
      })["catch"](function (e) {
        postMessage({id: data.id, type: "typeAtPos", err: e});
      });
      return;
    case "supportsParse":
      getFlow(data.version).then(function(flow) {
        var result = flow.parse != null;
        postMessage({id: data.id, type: "supportsParse", result: result});
      })["catch"](function (e) {
        postMessage({id: data.id, type: "supportsParse", err: e});
      });
      return;
    case "parse":
      getFlow(data.version).then(function(flow) {
        var result = flow.parse(
          data.body,
          data.options
        );
        postMessage({id: data.id, type: "parse", result: result});
      })["catch"](function (e) {
        postMessage({id: data.id, type: "parse", err: e});
      });
      return;
    default:
      throw new Error("Unknown message type: " + data.type);
  }
};

function getFlow(version) {
  if (!(version in versionCache)) {
    versionCache[version] = new Promise(function(resolve) {
      resolve(load(version));
    });
  }
  return versionCache[version];
}
//
// this.console = {
//   log: function(v) { postMessage({type: "debug", message: v}); }
// };
