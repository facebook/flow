import * as React from 'react';

export component C (x: number) { }

export default component D (y: number, ...props: {z: string}) { }

<C x={"a"} />; // error
<D y={"a"} /> // 2 errors
