/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import {load} from './flow-loader';

declare function postMessage(message: mixed): void;

const versionCache = {};

// $FlowFixMe[prop-missing]
this.onmessage = function (e) {
  var data = e.data;
  switch (data.type) {
    // preload flow. optional, but makes sure flow is ready
    case 'init':
      getFlow(data.version)
        .then(() => postMessage({id: data.id, type: 'init', result: true}))
        .catch(e => postMessage({id: data.id, type: 'init', err: e}));
      return;
    case 'checkContent':
      getFlow(data.version)
        .then(flow => {
          const result = flow.checkContent(data.filename, data.body);
          postMessage({id: data.id, type: 'checkContent', result: result});
        })
        .catch(e => postMessage({id: data.id, type: 'checkContent', err: e}));
      return;
    case 'typeAtPos':
      getFlow(data.version)
        .then(flow => {
          const result = flow.typeAtPos(
            data.filename,
            data.body,
            data.line,
            data.col,
          );
          postMessage({id: data.id, type: 'typeAtPos', result: result});
        })
        .catch(e => postMessage({id: data.id, type: 'typeAtPos', err: e}));
      return;
    case 'supportsParse':
      getFlow(data.version)
        .then(flow => {
          const result = flow.parse != null;
          postMessage({id: data.id, type: 'supportsParse', result: result});
        })
        .catch(e => postMessage({id: data.id, type: 'supportsParse', err: e}));
      return;
    case 'parse':
      getFlow(data.version)
        .then(flow => {
          const result = flow.parse(data.body, data.options);
          postMessage({id: data.id, type: 'parse', result: result});
        })
        .catch(e => postMessage({id: data.id, type: 'parse', err: e}));
      return;
    default:
      throw new Error('Unknown message type: ' + data.type);
  }
};

function getFlow(version) {
  if (!(version in versionCache)) {
    versionCache[version] = new Promise(resolve => resolve(load(version)));
  }
  return versionCache[version];
}

//
// this.console = {
//   log: function(v) { postMessage({type: "debug", message: v}); }
// };
