/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict
 * @format
 */

// flowlint unsafe-getters-setters:off

'use strict';

import type {
  BindingName,
  ESNode,
  Identifier,
  JSXIdentifier,
} from 'flow-estree-oxidized';
import type {Scope} from '../scope';
import type {Variable} from '../variable';

import {createIdGenerator} from '../ID';

const ReferenceFlag = ({
  Read: 0x1,
  Write: 0x2,
  ReadWrite: 0x3,
}: $ReadOnly<{
  Read: 0x1,
  Write: 0x2,
  ReadWrite: 0x3,
}>);
type ReferenceFlagType = $Values<typeof ReferenceFlag>;

type ReferenceImplicitGlobal = $ReadOnly<{
  node: ESNode,
  pattern: BindingName,
  ref?: Reference,
}>;

const generator = createIdGenerator();

const ReferenceTypeFlag = ({
  Value: 0x1,
  Type: 0x2,
  ValueAndType: 0x3,
}: $ReadOnly<{
  Value: 0x1,
  Type: 0x2,
  ValueAndType: 0x3,
}>);
type ReferenceTypeFlagType = $Values<typeof ReferenceTypeFlag>;

/**
 * A Reference represents a single occurrence of an identifier in code.
 */
class Reference {
  /**
   * A unique ID for this instance - primarily used to help debugging and testing
   */
  +$id: number = generator();
  /**
   * The read-write mode of the reference.
   */
  +_flag: ReferenceFlagType;
  /**
   * Reference to the enclosing Scope.
   * @public
   */
  +from: Scope;
  /**
   * Identifier syntax node.
   * @public
   */
  +identifier: Identifier | JSXIdentifier;
  /**
   * `true` if this writing reference is a variable initializer or a default value.
   * @public
   */
  +init: ?boolean;
  /**
   * The {@link Variable} object that this reference refers to. If such variable was not defined, this is `null`.
   * @public
   */
  resolved: Variable | null;
  /**
   * If reference is writeable, this is the node being written to it.
   * @public
   */
  +writeExpr: ?ESNode;

  +maybeImplicitGlobal: ?ReferenceImplicitGlobal;

  /**
   * In some cases, a reference may be a type, value or both a type and value reference.
   */
  +_referenceType: ReferenceTypeFlagType;

  /**
   * True if this reference can reference types
   * @public
   */
  get isTypeReference(): boolean {
    return (this._referenceType & ReferenceTypeFlag.Type) !== 0;
  }

  /**
   * True if this reference can reference values
   * @public
   */
  get isValueReference(): boolean {
    return (this._referenceType & ReferenceTypeFlag.Value) !== 0;
  }

  constructor(
    identifier: Identifier | JSXIdentifier,
    scope: Scope,
    flag: ReferenceFlagType,
    writeExpr?: ESNode | null,
    maybeImplicitGlobal?: ReferenceImplicitGlobal | null,
    init?: boolean,
    referenceType: ReferenceTypeFlagType = ReferenceTypeFlag.Value,
  ) {
    this.identifier = identifier;
    this.from = scope;
    this.resolved = null;
    this._flag = flag;

    if (this.isWrite()) {
      this.writeExpr = writeExpr;
      this.init = init;
    }

    this.maybeImplicitGlobal = maybeImplicitGlobal;
    this._referenceType = referenceType;
  }

  /**
   * Whether the reference is writeable.
   * @public
   */
  isWrite(): boolean {
    return !!(this._flag & ReferenceFlag.Write);
  }

  /**
   * Whether the reference is readable.
   * @public
   */
  isRead(): boolean {
    return !!(this._flag & ReferenceFlag.Read);
  }

  /**
   * Whether the reference is read-only.
   * @public
   */
  isReadOnly(): boolean {
    return this._flag === ReferenceFlag.Read;
  }

  /**
   * Whether the reference is write-only.
   * @public
   */
  isWriteOnly(): boolean {
    return this._flag === ReferenceFlag.Write;
  }

  /**
   * Whether the reference is read-write.
   * @public
   */
  isReadWrite(): boolean {
    return this._flag === ReferenceFlag.ReadWrite;
  }
}

export type {ReferenceImplicitGlobal, ReferenceFlagType, ReferenceTypeFlagType};
export {Reference, ReferenceFlag, ReferenceTypeFlag};
