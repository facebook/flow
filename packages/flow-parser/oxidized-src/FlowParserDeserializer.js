/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict
 * @format
 */

'use strict';

import type {HermesSourceLocation, HermesNode, HermesToken} from './HermesAST';
import type {FlowParserWASM} from './FlowParserWASM';
import type {ParserOptions} from './ParserOptions';
import type {BabelFile} from './babel/BabelAST';

import HermesParserDecodeUTF8String from './HermesParserDecodeUTF8String';
import NODE_DESERIALIZERS from './FlowParserNodeDeserializers';

type FlowComment = {
  type: 'Block' | 'Line' | 'CommentBlock' | 'CommentLine',
  loc: HermesSourceLocation,
  value: ?string,
  start?: number,
  end?: number,
};

type ExtendedPosition = {
  loc: HermesSourceLocation,
  start: number,
  end: number,
};

const LOCATION_IDENTIFIER_NAME = 1 << 0;

export type FlowParserProgram = {
  type: 'Program',
  loc: HermesSourceLocation,
  body: Array<?HermesNode>,
  comments: Array<FlowComment>,
  interpreter?: ?HermesNode,
  tokens?: Array<HermesToken>,
  errors?: Array<{loc: HermesSourceLocation, message: string}>,
  sourceType?: 'script' | 'module',
  docblock?: unknown,
  [string]: unknown,
};

/**
 * Deserializer for the Flow Rust parser's binary protocol.
 *
 * This is structurally identical to HermesParserDeserializer, but uses
 * FlowParserNodeDeserializers (matching the Rust serializer's node kind
 * numbering) and produces ESTree-compatible output directly.
 */
export default class FlowParserDeserializer {
  programBufferIdx: number;
  positionBufferIdx: number;
  readonly positionBufferSize: number;
  readonly stringBufferBase: number;
  readonly locMap: {[number]: HermesSourceLocation};
  readonly extendedPositions: Array<?ExtendedPosition>;
  readonly extendedRanges: WeakMap<HermesSourceLocation, ExtendedPosition>;
  readonly HEAPU8: FlowParserWASM['HEAPU8'];
  readonly HEAPU32: FlowParserWASM['HEAPU32'];
  readonly HEAPF64: FlowParserWASM['HEAPF64'];
  readonly options: ParserOptions;
  extendedLocationHeaders: boolean;

  readonly commentTypes: ReadonlyArray<FlowComment['type']> = ['Block', 'Line'];
  // Matches TokenType enum (same as Hermes for compatibility)
  readonly tokenTypes: ReadonlyArray<HermesToken['type']> = [
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
    programBuffer: number,
    positionBuffer: number,
    positionBufferSize: number,
    stringBufferBase: number,
    wasmParser: FlowParserWASM,
    options: ParserOptions,
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
    this.extendedPositions = [];
    this.extendedRanges = new WeakMap();

    this.HEAPU8 = wasmParser.HEAPU8;
    this.HEAPU32 = wasmParser.HEAPU32;
    this.HEAPF64 = wasmParser.HEAPF64;

    this.options = options;
    this.extendedLocationHeaders = false;
  }

  /**
   * Consume and return the next 4 bytes in the program buffer.
   */
  next(): number {
    const num = this.HEAPU32[this.programBufferIdx++];
    return num;
  }

  deserialize(): FlowParserProgram | BabelFile {
    if (this.HEAPU32[this.programBufferIdx] === 0) {
      return this.deserializeESTreeProgram();
    }
    this.extendedLocationHeaders = true;
    this.prepareExtendedPositions();
    const root = this.deserializeNode();
    if (root == null) {
      throw new Error('Expected serialized parser root');
    }
    // $FlowExpectedError[incompatible-type] The root node kind defines the public schema.
    return root;
  }

  deserializeESTreeProgram(): FlowParserProgram {
    const program: FlowParserProgram = {
      type: 'Program',
      loc: this.addEmptyLoc(),
      body: this.deserializeNodeList(),
      comments: this.deserializeESTreeComments(),
    };
    program.interpreter = this.deserializeNode();
    if (this.options.tokens === true) {
      program.tokens = this.deserializeTokens();
    } else {
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
  deserializeErrors(): Array<{loc: HermesSourceLocation, message: string}> {
    const size = this.next();
    const errors = [];
    for (let i = 0; i < size; i++) {
      const loc = this.addEmptyLoc();
      const message = this.deserializeString();
      if (message == null) {
        throw new Error('Expected serialized parser error message');
      }
      errors.push({loc, message});
    }
    return errors;
  }

  /**
   * Booleans are serialized as a single 4-byte integer.
   */
  deserializeBoolean(): boolean {
    return Boolean(this.next());
  }

  /**
   * Numbers are serialized directly into program buffer, taking up 8 bytes
   * preceded by 4 bytes of alignment padding if necessary.
   */
  deserializeNumber(): number {
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
  deserializeString(): ?string {
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
  deserializeNode(): ?HermesNode {
    const nodeType = this.next();
    if (nodeType === 0) {
      return null;
    }
    const deserializeNode = NODE_DESERIALIZERS[nodeType - 1];
    if (deserializeNode == null) {
      throw new Error(
        `Unknown serialized node kind ${nodeType - 1} at program word ${
          this.programBufferIdx - 1
        }`,
      );
    }
    const node = deserializeNode.call(this);
    if (this.extendedLocationHeaders) {
      this.addExtendedRange(node);
    }
    return node;
  }

  /**
   * Node lists are serialized as a 4-byte integer denoting the number of
   * elements in the list, followed by the serialized elements.
   */
  deserializeNodeList(): Array<?HermesNode> {
    const size = this.next();
    const nodeList = [];

    for (let i = 0; i < size; i++) {
      nodeList.push(this.deserializeNode());
    }
    return nodeList;
  }

  deserializeEnumRuntime(): unknown {
    const getRuntime =
      this.options.transformOptions?.TransformEnumSyntax?.getRuntime;
    if (typeof getRuntime !== 'function') {
      throw new Error('Expected TransformEnumSyntax.getRuntime callback');
    }
    return getRuntime();
  }

  /**
   * Comments are serialized as a node list, where each comment is serialized
   * as a 4-byte integer denoting comment type, followed by a 4-byte value
   * denoting the loc ID, followed by a serialized string for the comment value.
   */
  deserializeComments(): Array<FlowComment> {
    const size = this.next();
    const comments = [];

    for (let i = 0; i < size; i++) {
      const commentType = this.deserializeString();
      if (commentType == null) {
        throw new Error('Expected serialized comment type');
      }
      const loc = this.addEmptyLoc();
      const comment: FlowComment = {
        // $FlowExpectedError[incompatible-type] Rust emits the closed comment type set.
        type: commentType,
        loc,
        value: this.deserializeString(),
      };
      this.addExtendedRange(comment);
      comments.push(comment);
    }

    return comments;
  }

  deserializeESTreeComments(): Array<FlowComment> {
    const size = this.next();
    const comments = [];
    for (let i = 0; i < size; i++) {
      const commentType = this.commentTypes[this.next()];
      const comment: FlowComment = {
        type: commentType,
        loc: this.addEmptyLoc(),
        value: this.deserializeString(),
      };
      comments.push(comment);
    }
    return comments;
  }

  deserializeTokens(): Array<HermesToken> {
    const size = this.next();
    const tokens = [];

    for (let i = 0; i < size; i++) {
      const tokenType = this.tokenTypes[this.next()];
      const loc = this.addEmptyLoc();
      const value = this.deserializeString();
      const token: HermesToken = {
        type: tokenType,
        loc,
        value,
      };
      tokens.push(token);
    }

    return tokens;
  }

  /**
   * While deserializing the AST locations are represented by
   * a 4-byte loc ID. This is used to create a map of loc IDs to empty loc
   * objects that are filled after the AST has been deserialized.
   */
  addEmptyLoc(): HermesSourceLocation {
    const locId = this.next();
    if (this.extendedLocationHeaders) {
      const flags = this.next();
      const position = this.extendedPositions[locId];
      if (position == null) {
        throw new Error(`Missing serialized extended location ${locId}`);
      }
      const loc = position.loc;
      if (flags & LOCATION_IDENTIFIER_NAME) {
        const identifierName = this.deserializeString();
        if (identifierName != null) {
          // $FlowExpectedError[prop-missing] The extended wire location schema carries this field.
          loc.identifierName = identifierName;
        }
      }
      this.extendedRanges.set(loc, position);
      return loc;
    }
    const loc: HermesSourceLocation = {};
    this.locMap[locId] = loc;
    return loc;
  }

  prepareExtendedPositions(): void {
    let index = this.positionBufferIdx;
    for (let i = 0; i < this.positionBufferSize; i++) {
      const locId = this.HEAPU32[index++];
      const kind = this.HEAPU32[index++];
      const line = this.HEAPU32[index++];
      const column = this.HEAPU32[index++];
      const offset = this.HEAPU32[index++];
      const position: ExtendedPosition = this.extendedPositions[locId] ?? {
        loc: {} as HermesSourceLocation,
        start: 0,
        end: 0,
      };
      if (kind === 0) {
        position.loc.start = {line, column};
        position.start = offset;
      } else {
        position.loc.end = {line, column};
        position.end = offset;
      }
      this.extendedPositions[locId] = position;
    }
  }

  addExtendedRange(owner: {
    readonly loc: HermesSourceLocation,
    start?: number,
    end?: number,
    ...
  }): void {
    if (owner.loc == null) {
      return;
    }
    const position = this.extendedRanges.get(owner.loc);
    if (position == null) {
      return;
    }
    owner.start = position.start;
    owner.end = position.end;
  }

  /**
   * Positions are serialized as a loc ID which denotes which loc it is
   * associated with, followed by kind which denotes whether it is a start
   * or end position, followed by line, column, and offset (4-bytes each).
   */
  fillLocs(): void {
    for (let i = 0; i < this.positionBufferSize; i++) {
      const locId = this.HEAPU32[this.positionBufferIdx++];
      const kind = this.HEAPU32[this.positionBufferIdx++];
      const line = this.HEAPU32[this.positionBufferIdx++];
      const column = this.HEAPU32[this.positionBufferIdx++];
      const offset = this.HEAPU32[this.positionBufferIdx++];

      const loc = this.locMap[locId];
      if (loc == null) {
        throw new Error(`Missing serialized location ${locId}`);
      }
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
