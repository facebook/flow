/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import * as React from 'react';
import clsx from 'clsx';
import styles from './Reveal.module.css';

// useLayoutEffect on the client (so the hidden state is applied before the
// browser paints — no flash of fully-visible content), but fall back to
// useEffect during SSR where layout effects can't run.
const useIsomorphicLayoutEffect =
  typeof window !== 'undefined' ? React.useLayoutEffect : React.useEffect;

// Reveals its children once, as they scroll into view: a subtle fade + rise.
// SSR renders the content visible (state 'idle') so it's there with no JS and
// matches hydration; on mount we either leave it visible (reduced motion / no
// IntersectionObserver) or hide it and reveal on intersection.
export default component Reveal(
  children: React.Node,
  // Stagger this reveal after its siblings (ms).
  delay: number = 0,
  // Class for the wrapper element so it can stand in as a layout node (e.g. a
  // grid column) rather than introducing an extra nesting level.
  className?: string,
) {
  const ref = React.useRef<HTMLDivElement | null>(null);
  const [state, setState] = React.useState<'idle' | 'hidden' | 'shown'>('idle');

  useIsomorphicLayoutEffect(() => {
    const node = ref.current;
    if (node == null) {
      return;
    }
    const prefersReducedMotion =
      typeof window.matchMedia === 'function' &&
      window.matchMedia('(prefers-reduced-motion: reduce)').matches;
    if (prefersReducedMotion || typeof IntersectionObserver === 'undefined') {
      // Leave the content visible; no animation.
      return;
    }

    // Hide before paint, then reveal when the node scrolls into view.
    setState('hidden');
    const observer: IntersectionObserver = new IntersectionObserver(
      entries => {
        for (const entry of entries) {
          if (entry.isIntersecting) {
            setState('shown');
            observer.disconnect();
            break;
          }
        }
      },
      {threshold: 0.15, rootMargin: '0px 0px -10% 0px'},
    );
    observer.observe(node);
    return () => observer.disconnect();
  }, []);

  return (
    <div
      ref={ref}
      className={clsx(
        className,
        state === 'hidden' && styles.hidden,
        state === 'shown' && styles.shown,
      )}
      style={delay > 0 ? {transitionDelay: `${delay}ms`} : undefined}>
      {children}
    </div>
  );
}
