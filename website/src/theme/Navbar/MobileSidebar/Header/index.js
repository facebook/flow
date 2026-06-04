/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

// Swizzled from @docusaurus/theme-classic to render the color-mode toggle via
// NavbarThemeToggle (stable `flow-theme-toggle` class + hidden on the pages
// where theming is a no-op). Otherwise identical to the upstream header.

import * as React from 'react';
import {useNavbarMobileSidebar} from '@docusaurus/theme-common/internal';
import {translate} from '@docusaurus/Translate';
import IconClose from '@theme/Icon/Close';
import NavbarLogo from '@theme/Navbar/Logo';
import NavbarThemeToggle from '@site/src/components/NavbarThemeToggle';

function CloseButton() {
  const mobileSidebar = useNavbarMobileSidebar();
  return (
    <button
      type="button"
      aria-label={translate({
        id: 'theme.docs.sidebar.closeSidebarButtonAriaLabel',
        message: 'Close navigation bar',
        description: 'The ARIA label for close button of mobile sidebar',
      })}
      className="clean-btn navbar-sidebar__close"
      onClick={() => mobileSidebar.toggle()}>
      <IconClose color="var(--ifm-color-emphasis-600)" />
    </button>
  );
}

export default function NavbarMobileSidebarHeader(): React.Node {
  return (
    <div className="navbar-sidebar__brand">
      <NavbarLogo />
      <NavbarThemeToggle className="margin-right--md" />
      <CloseButton />
    </div>
  );
}
