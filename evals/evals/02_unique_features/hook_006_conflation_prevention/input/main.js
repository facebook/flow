/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import * as React from 'react';
import {useState, useEffect, useMemo} from 'react';

hook useMultiplier(x: number): number {
  const [factor, setFactor] = useState(1);
  useEffect(() => {
    setFactor(2);
  }, []);
  return x * factor;
}

hook useFormatter(x: number): string {
  return useMemo(() => '$' + x.toFixed(2), [x]);
}

function computePrices(
  prices: Array<number>,
  transform: (x: number) => number,
  format: (x: number) => string,
): Array<string> {
  return prices.map(p => format(transform(p)));
}

component PriceList(prices: Array<number>) {
  const results = computePrices(prices, useMultiplier, useFormatter);
  return (
    <ul>
      {results.map((r, i) => <li key={i}>{r}</li>)}
    </ul>
  );
}

component App() {
  const compute = useMultiplier;
  const result = compute(10);
  return (
    <div>
      <PriceList prices={[10, 20, 30]} />
      <span>{result}</span>
    </div>
  );
}
