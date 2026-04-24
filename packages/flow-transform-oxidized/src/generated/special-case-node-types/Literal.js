/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

import type {
  BigIntLiteral as BigIntLiteralType,
  BooleanLiteral as BooleanLiteralType,
  ESNode,
  NullLiteral as NullLiteralType,
  NumericLiteral as NumericLiteralType,
  RegExpLiteral as RegExpLiteralType,
  StringLiteral as StringLiteralType,
} from 'hermes-estree';
import type {DetachedNode} from '../../detachedNode';

import {detachedProps} from '../../detachedNode';

// Literals require a "raw" which is added by the estree transform, not hermes.

export type BigIntLiteralProps = {
  +value: $FlowFixMe /* bigint | null */,
  /**
   * Only set this if you want to use a source-code representation like 1_1n, etc.
   * By default "raw" will just be the exact number you've given.
   */
  +raw?: NumericLiteralType['raw'],
};
export function BigIntLiteral(props: {
  ...$ReadOnly<BigIntLiteralProps>,
  +parent?: ESNode,
}): DetachedNode<BigIntLiteralType> {
  return detachedProps<BigIntLiteralType>(props.parent, {
    type: 'Literal',
    value: props.value,
    raw: props.raw ?? `${props.value}n`,
    bigint: `${props.value}`,
  });
}

export type BooleanLiteralProps = {
  +value: BooleanLiteralType['value'],
};
export function BooleanLiteral(props: {
  ...$ReadOnly<BooleanLiteralProps>,
  +parent?: ESNode,
}): DetachedNode<BooleanLiteralType> {
  return detachedProps<BooleanLiteralType>(props.parent, {
    type: 'Literal',
    raw: props.value ? 'true' : 'false',
    value: props.value,
  });
}

export type NumericLiteralProps = {
  +value: NumericLiteralType['value'],
  /**
   * Only set this if you want to use a source-code representation like 1e100, 0x11, 1_1, etc.
   * By default "raw" will just be the exact number you've given.
   */
  +raw?: NumericLiteralType['raw'],
};
export function NumericLiteral(props: {
  ...$ReadOnly<NumericLiteralProps>,
  +parent?: ESNode,
}): DetachedNode<NumericLiteralType> {
  return detachedProps<NumericLiteralType>(props.parent, {
    type: 'Literal',
    value: props.value,
    raw: props.raw ?? `${props.value}`,
  });
}

export type NullLiteralProps = {};
export function NullLiteral(
  props: {
    +parent?: ESNode,
  } = {...null},
): DetachedNode<NullLiteralType> {
  return detachedProps<NullLiteralType>(props.parent, {
    type: 'Literal',
    value: null,
    raw: 'null',
  });
}

// pattern/flags are on a subobject in the estree spec, but are flat on the hermes types
// also the value is supposed to be a RegExp instance
export type RegExpLiteralProps = {
  +pattern: RegExpLiteralType['regex']['pattern'],
  +flags: RegExpLiteralType['regex']['flags'],
};
export function RegExpLiteral(props: {
  ...$ReadOnly<RegExpLiteralProps>,
  +parent?: ESNode,
}): DetachedNode<RegExpLiteralType> {
  const value = new RegExp(props.pattern, props.flags);
  return detachedProps<RegExpLiteralType>(props.parent, {
    type: 'Literal',
    value,
    raw: value.toString(),
    regex: {
      pattern: props.pattern,
      flags: props.flags,
    },
  });
}

export type StringLiteralProps = {
  +value: StringLiteralType['value'],
  +raw?: StringLiteralType['raw'],
};
export function StringLiteral(props: {
  ...$ReadOnly<StringLiteralProps>,
  +parent?: ESNode,
}): DetachedNode<StringLiteralType> {
  const hasSingleQuote = props.value.includes('"');
  const hasDoubleQuote = props.value.includes("'");
  let raw = props.raw;
  if (raw == null) {
    if (hasSingleQuote && hasDoubleQuote) {
      raw = `'${props.value.replace(/'/g, "\\'")}'`;
    } else if (hasSingleQuote) {
      raw = `"${props.value}"`;
    } else {
      raw = `'${props.value}'`;
    }
  }
  return detachedProps<StringLiteralType>(props.parent, {
    type: 'Literal',
    raw,
    value: props.value,
  });
}
