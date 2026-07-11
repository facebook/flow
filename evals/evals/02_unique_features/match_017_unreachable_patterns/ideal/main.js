/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type Severity = 'critical' | 'high' | 'medium' | 'low' | 'info';

/**
 * Compute incident response parameters based on severity.
 *
 * - critical: always escalate, notify up to 3x ticket count (max 50), SLA 1h
 * - high: escalate after 12h, notify 2x ticket count, SLA 4h
 * - medium: escalate after 24h, notify 1.5x ticket count (rounded up), SLA 8h
 * - low: never escalate, notify half the ticket count (at least 1), SLA 72h
 * - info: never escalate, no notifications, SLA 168h
 */
export function computeResponseAction(
  severity: Severity,
  ticketCount: number,
  hoursSinceReport: number,
): {escalate: boolean, notifyCount: number, slaHours: number} {
  return match (severity) {
    'critical' => {
      escalate: true,
      notifyCount: Math.min(ticketCount * 3, 50),
      slaHours: 1,
    },
    'high' => {
      escalate: hoursSinceReport > 12,
      notifyCount: ticketCount * 2,
      slaHours: 4,
    },
    'medium' => {
      escalate: hoursSinceReport > 24,
      notifyCount: Math.ceil(ticketCount * 1.5),
      slaHours: 8,
    },
    'low' => {
      escalate: false,
      notifyCount: Math.max(1, Math.floor(ticketCount * 0.5)),
      slaHours: 72,
    },
    'info' => {
      escalate: false,
      notifyCount: 0,
      slaHours: 168,
    },
  };
}
