/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import * as React from 'react';

export component Alert(severity: 'info' | 'warning' | 'error', message: string) {
  const label = severity === 'error' ? '[ERROR]' : severity === 'warning' ? '[WARN]' : '[INFO]';
  return (
    <div>
      <strong>{label}</strong>
      <p>{message}</p>
    </div>
  );
}

export component CompactAlert(
  severity: 'info' | 'warning' | 'error',
  message: string,
) renders Alert {
  const shortened = message.length > 50 ? message.slice(0, 50) + '...' : message;
  return <Alert severity={severity} message={shortened} />;
}

export component AlertStack(children: renders* Alert) {
  return <div>{children}</div>;
}

export component App() {
  return (
    <AlertStack>
      <Alert severity="info" message="System is running normally" />
      <Alert severity="error" message="Connection lost to database server" />
      <CompactAlert severity="warning" message="Memory usage is approaching the configured threshold limit for this service" />
    </AlertStack>
  );
}
