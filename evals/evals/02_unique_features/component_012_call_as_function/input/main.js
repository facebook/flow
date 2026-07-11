/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import * as React from 'react';

component Avatar(url: string, alt: string) {
  return <img src={url} alt={alt} />;
}

component UserRow(name: string, avatarUrl: string) {
  return (
    <div>
      {Avatar({url: avatarUrl, alt: name})}
      <span>{name}</span>
    </div>
  );
}
