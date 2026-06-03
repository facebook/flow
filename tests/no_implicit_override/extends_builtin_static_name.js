// Every class has a synthetic `static name` field auto-added by the class
// machinery with loc=None. Under NIO, walking the static side finds the
// inherited `name` on a real-source ancestor (e.g. flowlib `Error.name`),
// and the implicit-override check used to fire a spurious error pointing
// at the lib decl. The fix: skip emission when the subclass's own loc is
// None (synthetic — no user-facing decl to mark).

class MyError extends Error {} // OK — no spurious [invalid-override] on synthetic `name`

class MySubError extends MyError {} // OK — same, deeper in the chain

new MyError();
new MySubError();
