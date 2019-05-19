// @flow

import React from "react";

<p suppressHydrationWarning />; // OK
<p suppressHydrationWarning="false" />; // Error: Expected boolean
<p suppressHydrationWarning={false} />; // OK

<aside contentEditable suppressContentEditableWarning />; // OK
<aside contentEditable suppressContentEditableWarning="false" />; // Error: Expected boolean
<aside contentEditable suppressContentEditableWarning={false} />; // OK
