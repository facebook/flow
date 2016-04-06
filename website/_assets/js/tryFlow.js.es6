//= require codemirror/lib/codemirror
//= require codemirror/addon/lint/lint
//= require codemirror/mode/javascript/javascript
//= require codemirror/mode/xml/xml
//= require codemirror/mode/jsx/jsx
//= depend_on flow.js

import CodeMirror from "codemirror/lib/codemirror"
import flow from "flow"

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

var libs = [
  '/static/flowlib/core.js',
  // '/static/flowlib/bom.js',
  // '/static/flowlib/cssom.js',
  // '/static/flowlib/dom.js',
  // '/static/flowlib/node.js',
  // '/static/flowlib/react.js',
];

var flowReady = Promise.all(libs.map(get)).then(function(contents) {
  contents.forEach(function(nameAndContent) {
    flow.registerFile(nameAndContent[0], nameAndContent[1]);
  });
  return libs;
}).then(function(libs) {
  flow.setLibs(libs);
});

function printErrors(errors) {
  if (errors.length == 0) {
    return document.createTextNode('No errors!');
  }
  return errors.reduce((list, err) => {
    let li = document.createElement('li');
    li.innerHTML = JSON.stringify(err, null, 2);
    list.appendChild(li);
    return list;
  }, document.createElement('ul'));
}

function getAnnotations(text, callback, options) {
  const errorsNode = options.errorsNode;
  flowReady.then(function() {
    var errors = flow.checkContent('-', text);

    if (errorsNode) {
      while (errorsNode.lastChild) errorsNode.removeChild(errorsNode.lastChild);
      errorsNode.appendChild(printErrors(errors));
    }

    var lint = errors.map(function(err) {
      var messages = err.message;
      var firstLoc = messages[0].loc;
      var message = messages.map(function(msg) {
        return msg.descr;
      }).join("\n");
      return {
        from: CodeMirror.Pos(
          firstLoc.start.line - 1,
          firstLoc.start.column - 1
        ),
        to: CodeMirror.Pos(firstLoc.end.line - 1, firstLoc.end.column),
        severity: err.level,
        message: message
      };
    });
    callback(lint);
  });
}
getAnnotations.async = true;

exports.createEditor = function createEditor(domNode, errorsNode) {
  require([
    'codemirror/addon/lint/lint',
    'codemirror/mode/javascript/javascript',
    'codemirror/mode/xml/xml',
    'codemirror/mode/jsx/jsx'
  ], function() {
    CodeMirror.fromTextArea(domNode, {
      lineNumbers: true,
      mode: "jsx",
      lint: { getAnnotations, errorsNode }
    });
  });
}
