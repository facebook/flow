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

hook usePriceEntry(price: number): string {
  const multiplied = useMultiplier(price);
  const formatted = useFormatter(multiplied);
  return formatted;
}

component PriceItem(price: number) {
  const result = usePriceEntry(price);
  return <li>{result}</li>;
}

component PriceList(prices: Array<number>) {
  return (
    <ul>
      {prices.map((p, i) => <PriceItem key={i} price={p} />)}
    </ul>
  );
}

component App() {
  const result = useMultiplier(10);
  const formatted = useFormatter(result);
  return (
    <div>
      <PriceList prices={[10, 20, 30]} />
      <span>{formatted}</span>
    </div>
  );
}
