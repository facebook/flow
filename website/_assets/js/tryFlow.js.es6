//= require codemirror/lib/codemirror
//= require codemirror/addon/lint/lint
//= require codemirror/mode/javascript/javascript
//= require codemirror/mode/xml/xml
//= require codemirror/mode/jsx/jsx
//= require lz-string
//= depend_on flow.js

import CodeMirror from "codemirror/lib/codemirror"
import LZString from "lz-string"
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
  '/static/flowlib/bom.js',
  '/static/flowlib/cssom.js',
  '/static/flowlib/dom.js',
  '/static/flowlib/node.js',
  '/static/flowlib/react.js',
];

var flowReady = Promise.all(libs.map(get)).then(function(contents) {
  contents.forEach(function(nameAndContent) {
    flow.registerFile(nameAndContent[0], nameAndContent[1]);
  });
  return libs;
}).then(function(libs) {
  flow.setLibs(libs);
});

function printError(err, editor) {
  const clickHandler = (msg) => {
    editor.getDoc().setSelection(
      {line: msg.loc.start.line - 1, ch: msg.loc.start.column - 1},
      {line: msg.loc.end.line - 1, ch: msg.loc.end.column}
    );
    editor.focus();
  };

  return err.message.reduce((container, msg) => {
    if (msg.loc && msg.context != null) {
      const div = document.createElement('div');
      const filename = msg.loc.source !== '-' ? `${msg.loc.source}:` : '';
      const prefix = `${filename}${msg.loc.start.line}: `;

      const before = msg.context.slice(0, msg.loc.start.column - 1);
      const highlight = msg.context.slice(msg.loc.start.column - 1, msg.loc.end.column);
      const after = msg.context.slice(msg.loc.end.column);
      div.appendChild(document.createTextNode(prefix + before));
      const bold = document.createElement('strong');
      bold.className = "msgHighlight";
      bold.appendChild(document.createTextNode(highlight));
      div.appendChild(bold);
      div.appendChild(document.createTextNode(after));
      container.appendChild(div);

      const offset = msg.loc.start.column + prefix.length - 1;
      const arrow = `${(prefix + before).replace(/[^ ]/g, ' ')}^ `;
      container.appendChild(document.createTextNode(arrow));

      const span = document.createElement('span');
      span.className = "msgType";
      span.appendChild(document.createTextNode(msg.descr));
      container.appendChild(span);

      const handler = clickHandler.bind(null, msg);
      bold.addEventListener('click', handler);
      span.addEventListener('click', handler);
    } else {
      const descr = `. ${msg.descr}\n`;
      container.appendChild(document.createTextNode(descr));
    }
    return container;
  }, document.createElement('li'));
}

function printErrors(errors, editor) {
  if (errors.length == 0) {
    return document.createTextNode('No errors!');
  }
  return errors.reduce((list, err) => {
    list.appendChild(printError(err, editor));
    return list;
  }, document.createElement('ul'));
}

function getAnnotations(text, callback, options, editor) {
  const errorsNode = options.errorsNode;
  flowReady.then(function() {
    var errors = flow.checkContent('-', text);

    if (errorsNode) {
      while (errorsNode.lastChild) errorsNode.removeChild(errorsNode.lastChild);
      errorsNode.appendChild(printErrors(errors, editor));
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

const defaultValue = `/* @flow */

function foo(x: ?number): string {
  if (x) {
    return x;
  }
  return "default string";
}
`;

function getHashedValue(hash) {
  if (hash[0] !== '#' || hash.length < 2) return null;
  const version = hash.slice(1, 2);
  const encoded = hash.slice(2);
  if (version === '0' && encoded.match(/^[a-zA-Z0-9+/=_-]+$/)) {
    return LZString.decompressFromEncodedURIComponent(encoded);
  }
  return null;
}

exports.createEditor = function createEditor(domNode, errorsNode) {
  require([
    'codemirror/addon/lint/lint',
    'codemirror/mode/javascript/javascript',
    'codemirror/mode/xml/xml',
    'codemirror/mode/jsx/jsx'
  ], function() {
    const location = window.location;

    const editor = CodeMirror(domNode, {
      value: getHashedValue(location.hash) || defaultValue,
      autofocus: true,
      lineNumbers: true,
      mode: "jsx",
      lint: { getAnnotations, errorsNode }
    });

    editor.on('changes', () => {
      const value = editor.getValue();
      const encoded = LZString.compressToEncodedURIComponent(value);
      location.hash = `#0${encoded}`;
    });
  });
}
