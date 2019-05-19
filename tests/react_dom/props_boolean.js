// @flow

import React from "react";

<span hidden />; // OK
<span hidden={false} />; // OK
<span hidden={null} />; // Error: Expected DOM boolean
<span hidden="true" />; // Error: Expected DOM boolean
<span hidden="false" />; // Error: Expected DOM boolean
<span hidden="" />; // Error: Expected DOM boolean
<span hidden="yes" />; // Error: Expected DOM boolean
<span hidden="hidden" />; // OK

// FIXME: Case-insensitive match should be allowed, per HTML spec
<span hidden="Hidden" />; // Currently an error
