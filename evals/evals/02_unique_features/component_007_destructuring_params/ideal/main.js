/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import * as React from 'react';

export component BarChart(
  data: ReadonlyArray<{label: string, value: number}>,
  style as {barColor, barWidth}: Readonly<{barColor: string, barWidth: number}>,
) {
  return (
    <div>
      {data.map((point, i) => (
        <div key={i}>
          <span>{point.label}</span>
          <div
            style={{
              backgroundColor: barColor,
              width: barWidth * point.value + 'px',
              height: '20px',
            }}
          />
        </div>
      ))}
    </div>
  );
}

export component ScatterPlot(
  points: ReadonlyArray<{x: number, y: number}>,
  config as {dotRadius, dotColor, scale}: Readonly<{dotRadius: number, dotColor: string, scale: number}>,
) {
  return (
    <svg width={400} height={400}>
      {points.map((point, i) => (
        <circle
          key={i}
          cx={point.x * scale}
          cy={point.y * scale}
          r={dotRadius}
          fill={dotColor}
        />
      ))}
    </svg>
  );
}
