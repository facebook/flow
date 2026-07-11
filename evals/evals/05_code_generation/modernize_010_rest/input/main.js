/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type ButtonProps = {
  variant: string,
  size: string,
  disabled: boolean,
};

type StyleProps = {
  variant: string,
  size: string,
};

export function extractBehaviorProps(
  props: ButtonProps,
): $Rest<ButtonProps, StyleProps> {
  const {variant, size, ...rest} = props;
  return rest;
}
