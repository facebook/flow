"use strict";
exports.id = 5130;
exports.ids = [5130];
exports.modules = {

/***/ 5130:
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

// src/basic-languages/lexon/lexon.ts
var conf = {
  comments: {
    lineComment: "COMMENT"
  },
  brackets: [["(", ")"]],
  autoClosingPairs: [
    { open: "{", close: "}" },
    { open: "[", close: "]" },
    { open: "(", close: ")" },
    { open: '"', close: '"' },
    { open: ":", close: "." }
  ],
  surroundingPairs: [
    { open: "{", close: "}" },
    { open: "[", close: "]" },
    { open: "(", close: ")" },
    { open: "`", close: "`" },
    { open: '"', close: '"' },
    { open: "'", close: "'" },
    { open: ":", close: "." }
  ],
  folding: {
    markers: {
      start: new RegExp("^\\s*(::\\s*|COMMENT\\s+)#region"),
      end: new RegExp("^\\s*(::\\s*|COMMENT\\s+)#endregion")
    }
  }
};
var language = {
  tokenPostfix: ".lexon",
  ignoreCase: true,
  keywords: [
    "lexon",
    "lex",
    "clause",
    "terms",
    "contracts",
    "may",
    "pay",
    "pays",
    "appoints",
    "into",
    "to"
  ],
  typeKeywords: ["amount", "person", "key", "time", "date", "asset", "text"],
  operators: [
    "less",
    "greater",
    "equal",
    "le",
    "gt",
    "or",
    "and",
    "add",
    "added",
    "subtract",
    "subtracted",
    "multiply",
    "multiplied",
    "times",
    "divide",
    "divided",
    "is",
    "be",
    "certified"
  ],
  symbols: /[=><!~?:&|+\-*\/\^%]+/,
  tokenizer: {
    root: [
      [/^(\s*)(comment:?(?:\s.*|))$/, ["", "comment"]],
      [
        /"/,
        {
          token: "identifier.quote",
          bracket: "@open",
          next: "@quoted_identifier"
        }
      ],
      [
        "LEX$",
        {
          token: "keyword",
          bracket: "@open",
          next: "@identifier_until_period"
        }
      ],
      ["LEXON", { token: "keyword", bracket: "@open", next: "@semver" }],
      [
        ":",
        {
          token: "delimiter",
          bracket: "@open",
          next: "@identifier_until_period"
        }
      ],
      [
        /[a-z_$][\w$]*/,
        {
          cases: {
            "@operators": "operator",
            "@typeKeywords": "keyword.type",
            "@keywords": "keyword",
            "@default": "identifier"
          }
        }
      ],
      { include: "@whitespace" },
      [/[{}()\[\]]/, "@brackets"],
      [/[<>](?!@symbols)/, "@brackets"],
      [/@symbols/, "delimiter"],
      [/\d*\.\d*\.\d*/, "number.semver"],
      [/\d*\.\d+([eE][\-+]?\d+)?/, "number.float"],
      [/0[xX][0-9a-fA-F]+/, "number.hex"],
      [/\d+/, "number"],
      [/[;,.]/, "delimiter"]
    ],
    quoted_identifier: [
      [/[^\\"]+/, "identifier"],
      [/"/, { token: "identifier.quote", bracket: "@close", next: "@pop" }]
    ],
    space_identifier_until_period: [
      [":", "delimiter"],
      [" ", { token: "white", next: "@identifier_rest" }]
    ],
    identifier_until_period: [
      { include: "@whitespace" },
      [":", { token: "delimiter", next: "@identifier_rest" }],
      [/[^\\.]+/, "identifier"],
      [/\./, { token: "delimiter", bracket: "@close", next: "@pop" }]
    ],
    identifier_rest: [
      [/[^\\.]+/, "identifier"],
      [/\./, { token: "delimiter", bracket: "@close", next: "@pop" }]
    ],
    semver: [
      { include: "@whitespace" },
      [":", "delimiter"],
      [/\d*\.\d*\.\d*/, { token: "number.semver", bracket: "@close", next: "@pop" }]
    ],
    whitespace: [[/[ \t\r\n]+/, "white"]]
  }
};



/***/ })

};
;