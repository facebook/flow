// @flow

import React from 'react';

const {Suspense} = React;

function Loading() {
  return <div>Loading...</div>;
}

{
  <Suspense fallback={<Loading />} maxDuration="abc" /> // Error: string is incompatible with number
}

{
  <Suspense fallback={Loading} /> // Error: function is incompatible with exact React.Element
}

{
  <Suspense fallback={<Loading/>} />
}

{
  <Suspense fallback={<Loading/>} maxDuration={1000} />
}

{
  <Suspense fallback={<Loading/>} maxDuration={1000}>
    <div>Hello</div>
  </Suspense>
}

{
  <Suspense fallback={<Loading/>}>
    <Suspense />
  </Suspense>
}

{
  <Suspense fallback={<Loading/>}>
    <Suspense fallback={undefined} />
  </Suspense>
}

{
  <Suspense fallback={<Loading/>}>
    <Suspense fallback={null} />
  </Suspense>
}
