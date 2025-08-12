//@flow

import * as React from 'react';

const onRenderBad: React.ProfilerOnRenderFnType = ( // Error, the type isn't exported
  id: string,
  phase: "mount" | "update",
  actualDuration: number,
  baseDuration: number,
  startTime: number,
  commitTime: number,
  interactions: Set<React.Interaction>,
) => { return }

const onRender = () => {};
const _a = <React.Profiler id="Test1" onRender={onRender} />; // Profiler doesn't require children
const _b = <React.Profiler />; // Error, id and onRender are required


// Profiler doesn't preserve the instance type
function F(props: {||}) { return null }

const _c: ExactReactElement_DEPRECATED<
  // Ok, instance type is void.
  component(
    ...React.ElementConfig<typeof React.Profiler>
  )
>
=
  <React.Profiler id="Test3" onRender={onRender}>
    <F />
  </React.Profiler>;
