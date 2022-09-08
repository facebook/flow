// @flow

// This is a regression test for spurious FunctionThisLoc not prepared for type checking errors.
// It turns out that the following statement is parsed as ExportDefault (FunctionDeclaration {id=None, ...})
// Therefore, we do need to worry about the case where a function declaration does not have a name.
export default function () {}
