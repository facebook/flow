/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type FullProps = {
  id: string,
  label: string,
  onSelect: (id: string) => void,
};

type InjectedProps = {
  onSelect: (id: string) => void,
};

type OwnProps = Omit<FullProps, keyof InjectedProps>;

export function renderLabel(props: OwnProps): string {
  return props.label + ' #' + props.id;
}
