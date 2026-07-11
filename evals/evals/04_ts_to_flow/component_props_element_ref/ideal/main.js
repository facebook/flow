/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import * as React from 'react';

component Card(
  title: string,
  children: React.Node,
  footer?: React.Node,
  onDismiss?: () => void,
  ref: React.RefSetter<HTMLDivElement>,
) {
  return (
    <div ref={ref} className="card">
      <div className="card-header">
        <h2>{title}</h2>
        {onDismiss != null && (
          <button type="button" onClick={onDismiss}>
            Close
          </button>
        )}
      </div>
      <div className="card-body">{children}</div>
      {footer != null && <div className="card-footer">{footer}</div>}
    </div>
  );
}

export default Card;
