// @flow

import React from "react";

<header dir />; // Error: Expected enum
<header dir="ltr" />; // OK
<header dir="rtl" />; // OK
<header dir="auto" />; // OK

// FIXME: Case-insensitive match should be allowed, per HTML spec
<header dir="Ltr" />; // currently an error

// These likely variants are special-cased
<header dir="LTR" />; // OK
<header dir="RTL" />; // OK
