// @flow
// @nolint

import type NodeModuleTypes = require('./missing-type-only-module');
import type NodeAlias = NodeTypes.Value;
export import type ExportedNodeTypes = require('./missing-exported-type-only-module');
export as namespace FlowNodeTest;

type NodeType = number;
export {type NodeType};

class NodeFields {
  value?: number = 42;
  absent?: number;
}

var n: number = new NodeFields().value satisfies number;
console.log(n);
