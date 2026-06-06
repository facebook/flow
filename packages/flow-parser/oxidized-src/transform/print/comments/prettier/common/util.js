/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

'use strict';

const stringWidth = require('string-width');
const getLast = require('../utils/get-last.js');

const notAsciiRegex = /[^\x20-\x7F]/;

/**
 * @typedef {{backwards?: boolean}} SkipOptions
 */

/**
 * @param {string | RegExp} chars
 * @returns {(text: string, index: number | false, opts?: SkipOptions) => number | false}
 */
function skip(chars) {
  return (text, index, opts) => {
    const backwards = opts && opts.backwards;

    // Allow `skip` functions to be threaded together without having
    // to check for failures (did someone say monads?).
    /* istanbul ignore next */
    if (index === false) {
      return false;
    }

    const {length} = text;
    let cursor = index;
    while (cursor >= 0 && cursor < length) {
      const c = text.charAt(cursor);
      if (chars instanceof RegExp) {
        if (!chars.test(c)) {
          return cursor;
        }
      } else if (!chars.includes(c)) {
        return cursor;
      }

      backwards ? cursor-- : cursor++;
    }

    if (cursor === -1 || cursor === length) {
      // If we reached the beginning or end of the file, return the
      // out-of-bounds cursor. It's up to the caller to handle this
      // correctly. We don't want to indicate `false` though if it
      // actually skipped valid characters.
      return cursor;
    }
    return false;
  };
}

/**
 * @type {(text: string, index: number | false, opts?: SkipOptions) => number | false}
 */
const skipWhitespace = skip(/\s/);
/**
 * @type {(text: string, index: number | false, opts?: SkipOptions) => number | false}
 */
const skipSpaces = skip(' \t');
/**
 * @type {(text: string, index: number | false, opts?: SkipOptions) => number | false}
 */
const skipToLineEnd = skip(',; \t');
/**
 * @type {(text: string, index: number | false, opts?: SkipOptions) => number | false}
 */
const skipEverythingButNewLine = skip(/[^\n\r]/);

/**
 * @param {string} text
 * @param {number | false} index
 * @returns {number | false}
 */
function skipInlineComment(text, index) {
  /* istanbul ignore next */
  if (index === false) {
    return false;
  }

  if (text.charAt(index) === '/' && text.charAt(index + 1) === '*') {
    for (let i = index + 2; i < text.length; ++i) {
      if (text.charAt(i) === '*' && text.charAt(i + 1) === '/') {
        return i + 2;
      }
    }
  }
  return index;
}

/**
 * @param {string} text
 * @param {number | false} index
 * @returns {number | false}
 */
function skipTrailingComment(text, index) {
  /* istanbul ignore next */
  if (index === false) {
    return false;
  }

  if (text.charAt(index) === '/' && text.charAt(index + 1) === '/') {
    return skipEverythingButNewLine(text, index);
  }
  return index;
}

// This one doesn't use the above helper function because it wants to
// test \r\n in order and `skip` doesn't support ordering and we only
// want to skip one newline. It's simple to implement.
/**
 * @param {string} text
 * @param {number | false} index
 * @param {SkipOptions=} opts
 * @returns {number | false}
 */
function skipNewline(text, index, opts) {
  const backwards = opts && opts.backwards;
  if (index === false) {
    return false;
  }

  const atIndex = text.charAt(index);
  if (backwards) {
    // We already replace `\r\n` with `\n` before parsing
    /* istanbul ignore next */
    if (text.charAt(index - 1) === '\r' && atIndex === '\n') {
      return index - 2;
    }
    if (
      atIndex === '\n' ||
      atIndex === '\r' ||
      atIndex === '\u2028' ||
      atIndex === '\u2029'
    ) {
      return index - 1;
    }
  } else {
    // We already replace `\r\n` with `\n` before parsing
    /* istanbul ignore next */
    if (atIndex === '\r' && text.charAt(index + 1) === '\n') {
      return index + 2;
    }
    if (
      atIndex === '\n' ||
      atIndex === '\r' ||
      atIndex === '\u2028' ||
      atIndex === '\u2029'
    ) {
      return index + 1;
    }
  }

  return index;
}

/**
 * @param {string} text
 * @param {number} index
 * @param {SkipOptions=} opts
 * @returns {boolean}
 */
function hasNewline(text, index, opts = {}) {
  const idx = skipSpaces(text, opts.backwards ? index - 1 : index, opts);
  const idx2 = skipNewline(text, idx, opts);
  return idx !== idx2;
}

/**
 * @param {string} text
 * @param {number} start
 * @param {number} end
 * @returns {boolean}
 */
function hasNewlineInRange(text, start, end) {
  for (let i = start; i < end; ++i) {
    if (text.charAt(i) === '\n') {
      return true;
    }
  }
  return false;
}

/**
 * @param {string} text
 * @param {number} index
 * @returns {boolean}
 */
function isNextLineEmptyAfterIndex(text, index) {
  /** @type {number | false} */
  let oldIdx = null;
  /** @type {number | false} */
  let idx = index;
  while (idx !== oldIdx) {
    // We need to skip all the potential trailing inline comments
    oldIdx = idx;
    idx = skipToLineEnd(text, idx);
    idx = skipInlineComment(text, idx);
    idx = skipSpaces(text, idx);
  }
  idx = skipTrailingComment(text, idx);
  idx = skipNewline(text, idx);
  return idx !== false && hasNewline(text, idx);
}

/**
 * @param {string} text
 * @param {number} idx
 * @returns {number | false}
 */
function getNextNonSpaceNonCommentCharacterIndexWithStartIndex(text, idx) {
  /** @type {number | false} */
  let oldIdx = null;
  /** @type {number | false} */
  let nextIdx = idx;
  while (nextIdx !== oldIdx) {
    oldIdx = nextIdx;
    nextIdx = skipSpaces(text, nextIdx);
    nextIdx = skipInlineComment(text, nextIdx);
    nextIdx = skipTrailingComment(text, nextIdx);
    nextIdx = skipNewline(text, nextIdx);
  }
  return nextIdx;
}

/**
 * @template N
 * @param {string} text
 * @param {N} node
 * @param {(node: N) => number} locEnd
 * @returns {number | false}
 */
function getNextNonSpaceNonCommentCharacterIndex(text, node, locEnd) {
  return getNextNonSpaceNonCommentCharacterIndexWithStartIndex(
    text,
    locEnd(node),
  );
}

/**
 * @template N
 * @param {string} text
 * @param {N} node
 * @param {(node: N) => number} locEnd
 * @returns {string}
 */
function getNextNonSpaceNonCommentCharacter(text, node, locEnd) {
  return text.charAt(
    // @ts-expect-error => TBD: can return false, should we define a fallback?
    getNextNonSpaceNonCommentCharacterIndex(text, node, locEnd),
  );
}

/**
 * @param {string} text
 * @returns {number}
 */
function getStringWidth(text) {
  if (!text) {
    return 0;
  }

  // shortcut to avoid needless string `RegExp`s, replacements, and allocations within `string-width`
  if (!notAsciiRegex.test(text)) {
    return text.length;
  }

  return stringWidth(text);
}

function addCommentHelper(node, comment) {
  const comments = node.comments || (node.comments = []);
  comments.push(comment);
  comment.printed = false;
  comment.nodeDescription = describeNodeForDebugging(node);
}

function addLeadingComment(node, comment) {
  comment.leading = true;
  comment.trailing = false;
  addCommentHelper(node, comment);
}

function addDanglingComment(node, comment, marker) {
  comment.leading = false;
  comment.trailing = false;
  if (marker) {
    comment.marker = marker;
  }
  addCommentHelper(node, comment);
}

function addTrailingComment(node, comment) {
  comment.leading = false;
  comment.trailing = true;
  addCommentHelper(node, comment);
}

/**
 * @param {any} object
 * @returns {object is Array<any>}
 */
function isNonEmptyArray(object) {
  return Array.isArray(object) && object.length > 0;
}

function describeNodeForDebugging(node) {
  const nodeType = node.type || node.kind || '(unknown type)';
  let nodeName = String(
    node.name ||
      (node.id && (typeof node.id === 'object' ? node.id.name : node.id)) ||
      (node.key && (typeof node.key === 'object' ? node.key.name : node.key)) ||
      (node.value &&
        (typeof node.value === 'object' ? '' : String(node.value))) ||
      node.operator ||
      '',
  );
  if (nodeName.length > 20) {
    nodeName = nodeName.slice(0, 19) + '…';
  }
  return nodeType + (nodeName ? ' ' + nodeName : '');
}

module.exports = {
  getStringWidth,
  getLast,
  getNextNonSpaceNonCommentCharacterIndexWithStartIndex,
  getNextNonSpaceNonCommentCharacterIndex,
  getNextNonSpaceNonCommentCharacter,
  skipWhitespace,
  skipSpaces,
  skipNewline,
  isNextLineEmptyAfterIndex,
  hasNewline,
  hasNewlineInRange,
  addLeadingComment,
  addDanglingComment,
  addTrailingComment,
  isNonEmptyArray,
};
