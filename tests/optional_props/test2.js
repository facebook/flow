var a: { foo?: string } = {};
a.foo = undefined; // This is an error
a.foo = null; // Also an error

var b: { foo?: ?string } = {};
b.foo = undefined; // This is fine
b.foo = null; // Also fine

var c: { foo?: string } = { foo: undefined }; // This is an error
var d: { foo?: string } = { foo: null }; // Also is an error

var e: { foo?: ?string } = { foo: undefined }; // This is fine
var f: { foo?: ?string } = { foo: null }; // Also fine
