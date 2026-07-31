// `MyGlobal` is a global libdef declaration: it is visible in global scope
// without importing anything.
const ok: string = MyGlobal;
// It carries the declared type, so a mismatch points back at the libdef.
const bad: number = MyGlobal; // error: string ~> number

module.exports = {ok, bad};
