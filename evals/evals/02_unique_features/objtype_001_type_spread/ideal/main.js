/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 */

type BaseEvent = {
  id: string,
  timestamp: number,
  source: string,
};

type EnrichedEvent = {
  ...BaseEvent,
  tags: ReadonlyArray<string>,
  priority: number,
};

export function enrich(
  event: BaseEvent,
  tags: ReadonlyArray<string>,
  priority: number,
): EnrichedEvent {
  return {...event, tags, priority};
}
