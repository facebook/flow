/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import * as React from 'react';
import {useRef} from 'react';

export default component Player(src: string) {
  const videoRef = useRef<React.ElementRef<'video'> | null>(null);

  const restartMuted = () => {
    const video = videoRef.current;
    if (video != null) {
      video.currentTime = 0;
      video.muted = true;
    }
  };

  return (
    <figure>
      <video src={src} ref={videoRef} />
      <button type="button" onClick={restartMuted}>
        Restart muted
      </button>
    </figure>
  );
}
