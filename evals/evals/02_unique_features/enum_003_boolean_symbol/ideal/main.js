/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

export enum Toggle of boolean {
  On = true,
  Off = false,
}

export enum Priority of symbol {
  Low,
  Medium,
  High,
  Critical,
}

export function applyToggle(current: number, toggle: Toggle): number {
  switch (toggle) {
    case Toggle.On:
      return current * 2;
    case Toggle.Off:
      return 0;
  }
}

export function priorityWeight(p: Priority): number {
  switch (p) {
    case Priority.Low:
      return 1;
    case Priority.Medium:
      return 2;
    case Priority.High:
      return 4;
    case Priority.Critical:
      return 8;
  }
}

export function shouldAlert(p: Priority, toggle: Toggle): boolean {
  switch (toggle) {
    case Toggle.Off:
      return false;
    case Toggle.On:
      switch (p) {
        case Priority.High:
        case Priority.Critical:
          return true;
        case Priority.Low:
        case Priority.Medium:
          return false;
      }
      break;
  }
}
