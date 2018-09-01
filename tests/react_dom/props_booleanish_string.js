// @flow

import React from "react";

<div contentEditable />; // OK
<div contentEditable={false} />; // OK
<div contentEditable={null} />; // Error: Expected booleanish string
<div contentEditable="true" />; // OK
<div contentEditable="false" />; // OK
<div contentEditable="" />; // Error: Expected booleanish string
<div contentEditable="yes" />; // Error: Expected booleanish string
<div contentEditable="contentEditable" />; // Error: Expected booleanish string
