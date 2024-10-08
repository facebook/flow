// bad error: duplicate module
export const foo: string = ''; // weird but ok: no conformance error against .ios.js.flow because this file is completely shadowed by js.flow file, but against .js.flow
