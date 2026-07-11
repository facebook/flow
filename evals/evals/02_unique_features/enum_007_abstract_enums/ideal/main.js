/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

export enum Color {
  Red,
  Green,
  Blue,
}

export enum Size {
  Small,
  Medium,
  Large,
}

export function enumToArray<T extends EnumValue<string>>(
  e: Enum<T>,
): Array<{label: string, value: string}> {
  return Array.from(e.members(), member => ({
    label: e.getName(member),
    value: member.valueOf(),
  }));
}

export function safeParseEnum<T extends EnumValue<string>>(
  e: Enum<T>,
  input: string,
): T | null {
  return e.cast(input) ?? null;
}

export function enumIncludes<T extends EnumValue<string>>(
  e: Enum<T>,
  input: string,
): boolean {
  return e.isValid(input);
}
