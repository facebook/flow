myWeirdWindowScopedGlobal; // cannot-resolve-name
globalThis.myWeirdGlobalScopedGlobal; // prop-missing
window.myWeirdWindowScopedGlobal as empty; // error: string ~> empty

window.myGlobal as Opaque; // ok
window.myGlobal as globalThis.Opaque; // ok: this is weird, but TS also supports this
window.myGlobal as empty; // error
globalThis.myGlobal as empty; // error

Array as globalThis.React.Node; // error
