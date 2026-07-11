/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import * as React from 'react';

export component Badge(text: string) {
  return <span>{text}</span>;
}

export component StatusBadge(ok: boolean) renders Badge {
  return <Badge text={ok ? 'OK' : 'FAIL'} />;
}

export component RequiredSlot(badge: renders Badge) {
  return <div>{badge}</div>;
}

export component OptionalSlot(badge: renders? Badge) {
  return <div>{badge}</div>;
}

export component ManySlot(badges: renders* Badge) {
  return <div>{badges}</div>;
}

declare const maybeBadge: renders? Badge;

export component App() {
  const one = <StatusBadge ok={true} />;
  return (
    <div>
      <RequiredSlot badge={one} />
      <OptionalSlot badge={one} />
      <ManySlot badges={one} />
      <ManySlot badges={maybeBadge} />
    </div>
  );
}
