/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import * as React from 'react';
import {useState, useEffect} from 'react';

hook useAnimatedValue(target: number, speed: number): number {
  const [current, setCurrent] = useState(target);
  useEffect(() => {
    if (current === target) {
      return;
    }
    const step = current < target ? speed : -speed;
    const timer = setTimeout(() => {
      const next = current + step;
      const clamped = step > 0
        ? Math.min(next, target)
        : Math.max(next, target);
      setCurrent(clamped);
    }, 16);
    return () => clearTimeout(timer);
  }, [current, target, speed]);
  return current;
}

hook useFetchedData(url: string): string | null {
  const [data, setData] = useState<string | null>(null);
  useEffect(() => {
    let cancelled = false;
    fetch(url).then(r => r.text()).then(text => {
      if (!cancelled) {
        setData(text);
      }
    });
    return () => { cancelled = true; };
  }, [url]);
  return data;
}

component AnimatedBar(value: number, animated: boolean) {
  const animatedValue = useAnimatedValue(value, 2);
  const displayValue = animated ? animatedValue : value;
  const width = Math.max(0, Math.min(displayValue, 100));
  return <div style={{width: width + '%', height: '20px', backgroundColor: 'blue'}} />;
}

component DataPanel(url: string, enabled: boolean) {
  const data = useFetchedData(url);
  const content = enabled ? (data ?? 'Loading...') : 'Disabled';
  return <div>{content}</div>;
}

export component Dashboard(animated: boolean, dataEnabled: boolean) {
  return (
    <div>
      <AnimatedBar value={75} animated={animated} />
      <DataPanel url="https://api.example.com/status" enabled={dataEnabled} />
    </div>
  );
}
