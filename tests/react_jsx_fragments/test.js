import * as React from 'react';

<React.Fragment />;

< ></>; // success

<></>; // success

<>hi</>; // success

<><span>hi</span><div>bye</div></>; // success

< // a comment
/* another comment */
>hi</>; // success
