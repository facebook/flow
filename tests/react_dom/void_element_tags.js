// @flow

import React from "react";

<menuitem />; // OK
<br />; // OK
<menuitem dangerouslySetInnerHTML={{ __html: "Content" }} />; // Error
<br>Content</br>; // Error
<menuitem dangerouslySetInnerHTML={null} />; // OK
<br>{null}</br>; // OK
