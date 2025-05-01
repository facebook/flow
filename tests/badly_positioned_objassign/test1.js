class C { p: string };
// $FlowExpectedError[unsafe-object-assign]
Object.assign({ p: 0 }, new C);
