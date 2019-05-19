// @flow

import React from "react";

<article dangerouslySetInnerHTML />; // Error: Expected object
<article dangerouslySetInnerHTML="<blink>Hello</blink>" />; // Error: Expected object
<article dangerouslySetInnerHTML={{ __html: "<blink>Hello</blink>" }} />; // OK
<article dangerouslySetInnerHTML={{ html: "<blink>Hello</blink>" }} />; // Error: Missing __html
