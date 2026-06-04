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
import {useLocation} from '@docusaurus/router';
import useBaseUrl from '@docusaurus/useBaseUrl';
import NavbarColorModeToggle from '@theme/Navbar/ColorModeToggle';

// The landing page and the Try playground render identically in light and dark,
// so the theme switch does nothing visible there — and a control that appears to
// do nothing reads as broken. On those two routes we render nothing; everywhere
// else, the normal color-mode toggle, tagged with a stable `flow-theme-toggle`
// class so the navbar/drawer can position it without DOM-structure guesswork.
function useHideThemeToggle(): boolean {
  const {pathname} = useLocation();
  const home = useBaseUrl('/');
  const tryFlow = useBaseUrl('/try');
  const strip = (p: string) => (p.length > 1 ? p.replace(/\/+$/, '') : p);
  const here = strip(pathname);
  return here === strip(home) || here === strip(tryFlow);
}

export default function NavbarThemeToggle({
  className,
}: {
  className?: string,
}): React.Node {
  if (useHideThemeToggle()) {
    return null;
  }
  return (
    <NavbarColorModeToggle className={clsx('flow-theme-toggle', className)} />
  );
}
