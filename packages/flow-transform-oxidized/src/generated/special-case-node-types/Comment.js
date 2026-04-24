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
  BlockComment as BlockCommentType,
  LineComment as LineCommentType,
} from 'hermes-estree';

import {detachedProps} from '../../detachedNode';

export type LineCommentProps = {+value: string};
export function LineComment(props: LineCommentProps): LineCommentType {
  // $FlowExpectedError[prop-missing]
  // $FlowExpectedError[incompatible-type]
  return detachedProps<LineCommentType>(undefined, {
    type: 'Line',
    value: props.value,
  });
}

export type BlockCommentProps = {+value: string};
export function BlockComment(props: BlockCommentProps): BlockCommentType {
  // $FlowExpectedError[prop-missing]
  // $FlowExpectedError[incompatible-type]
  return detachedProps<BlockCommentType>(undefined, {
    type: 'Block',
    value: props.value,
  });
}
