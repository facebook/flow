/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

'use strict';

const HermesParserDecodeUTF8String = require('./HermesParserDecodeUTF8String');
const NODE_DESERIALIZERS = require('./FlowParserNodeDeserializers');

/**
 * Deserializer for the Flow Rust parser's binary protocol.
 *
 * This is structurally identical to HermesParserDeserializer, but uses
 * FlowParserNodeDeserializers (matching the Rust serializer's node kind
 * numbering) and produces ESTree-compatible output directly.
 */
class FlowParserDeserializer {
  // Comment types: Flow uses ESTree-standard names
  // Matches CommentKind enum in ast.rs: Block = 0, Line = 1
  commentTypes = ['Block', 'Line'];

  // Matches TokenType enum (same as Hermes for compatibility)
  tokenTypes = [
    'Boolean',
    'Identifier',
    'Keyword',
    'Null',
    'Numeric',
    'BigInt',
    'Punctuator',
    'String',
    'RegularExpression',
    'Template',
    'JSXText',
  ];

  constructor(
    programBuffer,
    positionBuffer,
    positionBufferSize,
    stringBufferBase,
    wasmParser,
    options,
  ) {
    // Program and position buffer are memory addresses, so we must convert
    // into indices into HEAPU32 (an array of 4-byte integers).
    this.programBufferIdx = programBuffer / 4;
    this.positionBufferIdx = positionBuffer / 4;

    this.positionBufferSize = positionBufferSize;
    // Base byte address into the WASM heap for the side string buffer.
    // Each non-null string is encoded in the program buffer as
    // `(offset+1, len)`; we decode UTF-8 starting at `stringBufferBase +
    // offset`. The +1 disambiguates an empty string at offset 0 from a
    // null pointer (encoded as `(0,)` with no length word).
    this.stringBufferBase = stringBufferBase;
    this.locMap = {};

    this.HEAPU8 = wasmParser.HEAPU8;
    this.HEAPU32 = wasmParser.HEAPU32;
    this.HEAPF64 = wasmParser.HEAPF64;

    this.options = options;
  }

  /**
   * Consume and return the next 4 bytes in the program buffer.
   */
  next() {
    const num = this.HEAPU32[this.programBufferIdx++];
    return num;
  }

  deserialize() {
    const program = {
      type: 'Program',
      loc: this.addEmptyLoc(),
      body: this.deserializeNodeList(),
      comments: this.deserializeComments(),
    };

    // Interpreter directive (OCaml `program` estree_translator.ml:118-123).
    // The serializer writes a Node slot here: an `InterpreterDirective` node
    // when `#!shebang` is present, otherwise null. Only attach to the program
    // when present, matching the OCaml shape.
    const interpreter = this.deserializeNode();
    if (interpreter != null) {
      program.interpreter = interpreter;
    }

    if (this.options.tokens === true) {
      program.tokens = this.deserializeTokens();
    } else {
      // Tokens slot is always written by the serializer; consume the count
      // even when callers didn't ask for tokens.
      this.deserializeTokens();
    }

    program.errors = this.deserializeErrors();

    this.fillLocs();

    return program;
  }

  /**
   * Errors are serialized as a 4-byte count followed by (locId, message)
   * pairs. Each entry becomes `{loc, message}` matching the OCaml/Hermes
   * shape consumed by the hardcoded fixture comparator.
   */
  deserializeErrors() {
    const size = this.next();
    const errors = [];
    for (let i = 0; i < size; i++) {
      const loc = this.addEmptyLoc();
      const message = this.deserializeString();
      errors.push({loc, message});
    }
    return errors;
  }

  /**
   * Booleans are serialized as a single 4-byte integer.
   */
  deserializeBoolean() {
    return Boolean(this.next());
  }

  /**
   * Numbers are serialized directly into program buffer, taking up 8 bytes
   * preceded by 4 bytes of alignment padding if necessary.
   */
  deserializeNumber() {
    let floatIdx;

    // Numbers are aligned on 8-byte boundaries, so skip padding if we are at
    // an odd index into the 4-byte aligned program buffer.
    if (this.programBufferIdx % 2 === 0) {
      floatIdx = this.programBufferIdx / 2;
      this.programBufferIdx += 2;
    } else {
      floatIdx = (this.programBufferIdx + 1) / 2;
      this.programBufferIdx += 3;
    }

    return this.HEAPF64[floatIdx];
  }

  /**
   * Strings are serialized as `(offset+1, len)` where `offset` is the byte
   * position into the side `string_buffer`. A 0 in the first slot means the
   * string is null and no length word follows. The `+1` lets an empty
   * string at offset 0 stay distinguishable from null.
   */
  deserializeString() {
    const offsetPlusOne = this.next();
    if (offsetPlusOne === 0) {
      return null;
    }

    const size = this.next();

    return HermesParserDecodeUTF8String(
      this.stringBufferBase + offsetPlusOne - 1,
      size,
      this.HEAPU8,
    );
  }

  /**
   * Nodes are serialized as a 4-byte integer denoting their node kind,
   * followed by a 4-byte loc ID, followed by serialized node properties.
   *
   * If the node kind is 0 the node is null, otherwise the node kind - 1 is an
   * index into the array of node deserialization functions.
   */
  deserializeNode() {
    const nodeType = this.next();
    if (nodeType === 0) {
      return null;
    }

    const nodeDeserializer = NODE_DESERIALIZERS[nodeType - 1].bind(this);
    return nodeDeserializer();
  }

  /**
   * Node lists are serialized as a 4-byte integer denoting the number of
   * elements in the list, followed by the serialized elements.
   */
  deserializeNodeList() {
    const size = this.next();
    const nodeList = [];

    for (let i = 0; i < size; i++) {
      nodeList.push(this.deserializeNode());
    }

    return nodeList;
  }

  /**
   * Comments are serialized as a node list, where each comment is serialized
   * as a 4-byte integer denoting comment type, followed by a 4-byte value
   * denoting the loc ID, followed by a serialized string for the comment value.
   */
  deserializeComments() {
    const size = this.next();
    const comments = [];

    for (let i = 0; i < size; i++) {
      const commentType = this.commentTypes[this.next()];
      const loc = this.addEmptyLoc();
      const value = this.deserializeString();
      comments.push({
        type: commentType,
        loc,
        value,
      });
    }

    return comments;
  }

  deserializeTokens() {
    const size = this.next();
    const tokens = [];

    for (let i = 0; i < size; i++) {
      const tokenType = this.tokenTypes[this.next()];
      const loc = this.addEmptyLoc();
      const value = this.deserializeString();
      tokens.push({
        type: tokenType,
        loc,
        value,
      });
    }

    return tokens;
  }

  /**
   * While deserializing the AST locations are represented by
   * a 4-byte loc ID. This is used to create a map of loc IDs to empty loc
   * objects that are filled after the AST has been deserialized.
   */
  addEmptyLoc() {
    const loc = {};
    this.locMap[this.next()] = loc;
    return loc;
  }

  /**
   * Positions are serialized as a loc ID which denotes which loc it is
   * associated with, followed by kind which denotes whether it is a start
   * or end position, followed by line, column, and offset (4-bytes each).
   */
  fillLocs() {
    for (let i = 0; i < this.positionBufferSize; i++) {
      const locId = this.HEAPU32[this.positionBufferIdx++];
      const kind = this.HEAPU32[this.positionBufferIdx++];
      const line = this.HEAPU32[this.positionBufferIdx++];
      const column = this.HEAPU32[this.positionBufferIdx++];
      const offset = this.HEAPU32[this.positionBufferIdx++];

      const loc = this.locMap[locId];
      if (kind === 0) {
        loc.start = {
          line,
          column,
        };
        loc.rangeStart = offset;
      } else {
        loc.end = {
          line,
          column,
        };
        loc.rangeEnd = offset;
      }
    }
  }
}

module.exports = FlowParserDeserializer;
