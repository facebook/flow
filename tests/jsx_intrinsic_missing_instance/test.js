import * as React from 'react';

const ref = React.createRef<unknown>();
<noinstance ref={ref} />; // Error: the intrinsic has no `instance`
