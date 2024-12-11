"use strict";
exports.id = 8390;
exports.ids = [8390];
exports.modules = {

/***/ 98390:
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   conf: () => (/* binding */ conf),
/* harmony export */   language: () => (/* binding */ language)
/* harmony export */ });
/*!-----------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Version: 0.34.0(9d278685b078158491964f8fd7ac9628fffa0f30)
 * Released under the MIT license
 * https://github.com/microsoft/monaco-editor/blob/main/LICENSE.txt
 *-----------------------------------------------------------------------------*/

// src/basic-languages/pla/pla.ts
var conf = {
  comments: {
    lineComment: "#"
  },
  brackets: [
    ["[", "]"],
    ["<", ">"],
    ["(", ")"]
  ],
  autoClosingPairs: [
    { open: "[", close: "]" },
    { open: "<", close: ">" },
    { open: "(", close: ")" }
  ],
  surroundingPairs: [
    { open: "[", close: "]" },
    { open: "<", close: ">" },
    { open: "(", close: ")" }
  ]
};
var language = {
  defaultToken: "",
  tokenPostfix: ".pla",
  brackets: [
    { open: "[", close: "]", token: "delimiter.square" },
    { open: "<", close: ">", token: "delimiter.angle" },
    { open: "(", close: ")", token: "delimiter.parenthesis" }
  ],
  keywords: [
    ".i",
    ".o",
    ".mv",
    ".ilb",
    ".ob",
    ".label",
    ".type",
    ".phase",
    ".pair",
    ".symbolic",
    ".symbolic-output",
    ".kiss",
    ".p",
    ".e",
    ".end"
  ],
  comment: /#.*$/,
  identifier: /[a-zA-Z]+[a-zA-Z0-9_\-]*/,
  plaContent: /[01\-~\|]+/,
  tokenizer: {
    root: [
      { include: "@whitespace" },
      [/@comment/, "comment"],
      [
        /\.([a-zA-Z_\-]+)/,
        {
          cases: {
            "@eos": { token: "keyword.$1" },
            "@keywords": {
              cases: {
                ".type": { token: "keyword.$1", next: "@type" },
                "@default": { token: "keyword.$1", next: "@keywordArg" }
              }
            },
            "@default": { token: "keyword.$1" }
          }
        }
      ],
      [/@identifier/, "identifier"],
      [/@plaContent/, "string"]
    ],
    whitespace: [[/[ \t\r\n]+/, ""]],
    type: [{ include: "@whitespace" }, [/\w+/, { token: "type", next: "@pop" }]],
    keywordArg: [
      [
        /[ \t\r\n]+/,
        {
          cases: {
            "@eos": { token: "", next: "@pop" },
            "@default": ""
          }
        }
      ],
      [/@comment/, "comment", "@pop"],
      [
        /[<>()\[\]]/,
        {
          cases: {
            "@eos": { token: "@brackets", next: "@pop" },
            "@default": "@brackets"
          }
        }
      ],
      [
        /\-?\d+/,
        {
          cases: {
            "@eos": { token: "number", next: "@pop" },
            "@default": "number"
          }
        }
      ],
      [
        /@identifier/,
        {
          cases: {
            "@eos": { token: "identifier", next: "@pop" },
            "@default": "identifier"
          }
        }
      ],
      [
        /[;=]/,
        {
          cases: {
            "@eos": { token: "delimiter", next: "@pop" },
            "@default": "delimiter"
          }
        }
      ]
    ]
  }
};



/***/ })

};
;