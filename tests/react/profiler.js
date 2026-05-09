import React from 'react';

{
  const {Profiler} = React;

  declare function onRender(
    id: string,
    phase: "mount" | "update",
    actualDuration: number,
    baseDuration: number,
    startTime: number,
    commitTime: number,
    interactions: Set<React.Interaction>,
  ): void;

  <Profiler id="fake" onRender={onRender} />;
  <Profiler id="fake" onRender={onRender}>
    <div />
  </Profiler>;
}

{
  const {Profiler} = React;

  declare function onRender(
    id: string,
    phase: "mount" | "update",
    actualDuration: number,
    baseDuration: number,
    startTime: number,
    commitTime: number,
    interactions: Set<React.Interaction>,
  ): void;

  <Profiler />; // Error: no "id" or "onRender" prop
  <Profiler id="fake" />; // Error: no "onRender" prop
  <Profiler onRender={onRender} />; // Error: no "id" prop
  <Profiler id="fake" onRender={null} />; // Error: invalid "onRender" prop

  declare function badOnRender(foo: number): void;

  <Profiler id="fake" onRender={badOnRender} />; // Error: invalid "onRender" prop
}
