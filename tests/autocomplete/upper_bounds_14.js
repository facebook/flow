//@flow

import * as React from "react";

type U = "success" | "failure";

type Props = {
  status: U;
}

declare function StatusIcon(props: Props): React.Node;

<StatusIcon status=''/>
//                  ^
