// @flow

import React from "react";

<h1 tabIndex />; // Error: Expected number
<h1 tabIndex={-1} />; // OK
<h1 tabIndex={0} />; // OK
<h1 tabIndex={1} />; // OK

<h1 tabIndex="foo" />; // Error: Expected number
<h1 tabIndex="1" />; // Error: Expected number
