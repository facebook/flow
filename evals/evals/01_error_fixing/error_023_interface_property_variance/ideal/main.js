/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 */

// A dashboard reads gauge samples through this view. The `value` slot is wide
// because different sensors expose numeric readings and short status strings
// ("OK", "FAULT") through the same field.
interface GaugeView {
  readonly label: string;
  readonly value: number | string;
}

// A temperature sensor only ever produces a numeric reading.
type TemperatureSample = {
  label: string,
  value: number,
};

function formatGauge(view: GaugeView): string {
  const displayed =
    typeof view.value === 'number' ? view.value.toFixed(1) : view.value;
  return view.label + ': ' + displayed;
}

function renderTemperatures(samples: Array<TemperatureSample>): Array<string> {
  const lines: Array<string> = [];
  for (const sample of samples) {
    const view: GaugeView = sample;
    lines.push(formatGauge(view));
  }
  return lines;
}

const readings: Array<TemperatureSample> = [
  {label: 'inlet', value: 72.4},
  {label: 'outlet', value: 68.1},
];

renderTemperatures(readings);
