// @flow

import React from 'react';

{
  const {Profiler} = React;

  function onRender(
    id: string,
    phase: "mount" | "update",
    actualDuration: number,
    baseDuration: number,
    startTime: number,
    commitTime: number,
    interactions: Set,
  ) {
    // Dummy callback
  }

  <Profiler id="fake" onRender={onRender} />
  <Profiler id="fake" onRender={onRender}>
    <div />
  </Profiler>
}

{
  const {Profiler} = React;

  <Profiler /> // Error: no "id" or "onRender" prop
  <Profiler id="fake" /> // Error: no "onRender" prop
  <Profiler onRender={onRender} /> // Error: no "id" prop
  <Profiler id="fake" onRender={null} /> // Error: invalid "onRender" prop

  function badOnRender(foo: number) {}

  <Profiler id="fake" onRender={badOnRender} /> // Error: invalid "onRender" prop
}
