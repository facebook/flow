/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

export enum EventType of string {
  PageView = 'page_view',
  Click = 'click',
  FormSubmit = 'form_submit',
  Dismiss = 'dismiss',
}

export function parseEvent(wire: string): EventType | void {
  return EventType.cast(wire);
}

export function toWire(event: EventType): string {
  return event as string;
}

export function isInteractive(event: EventType): boolean {
  return event === EventType.Click || event === EventType.FormSubmit;
}
