//@flow
import * as React from 'react';

const RC = <T />

function T(): void {}

declare function Wrap({children: [mixed, mixed]}): React.Node;

(<Wrap>
  {/* ok */ true ? <div></div> : <div></div>}
  <div></div>
</Wrap>);
