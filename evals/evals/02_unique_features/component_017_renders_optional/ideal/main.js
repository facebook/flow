/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import * as React from 'react';

export component Toast(message: string) {
  return <div>{message}</div>;
}

export component NotificationArea(toast: renders? Toast) {
  return <div>{toast}</div>;
}

export component MaybeToast(message: string, visible: boolean) renders? Toast {
  return visible ? <Toast message={message} /> : null;
}

export component App() {
  return (
    <div>
      <NotificationArea toast={<Toast message="Saved!" />} />
      <NotificationArea toast={null} />
      <NotificationArea toast={<MaybeToast message="Maybe later" visible={false} />} />
    </div>
  );
}
