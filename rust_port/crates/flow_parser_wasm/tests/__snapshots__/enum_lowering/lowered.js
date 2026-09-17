const SymbolE = require("flow-enums-runtime")(
  {
    A: Symbol("A"),
    B: Symbol("B"),
  },
);
const MirroredE = require("flow-enums-runtime").Mirrored(["C", "D"]);
export default MirroredE;
