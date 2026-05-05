import { useHook, useHooklikeAnnotatedFunction, useDeclaredHooklikeFunction, useAssignedHooklikeArbitraryExpression, useAssignedHooklikeFunctionExpression} from "./syntax_compatibility"

useHook(); // Error
useHooklikeAnnotatedFunction(); // Ok
useDeclaredHooklikeFunction(); // Ok
useAssignedHooklikeArbitraryExpression(); // Ok
useAssignedHooklikeFunctionExpression(); // Ok

component C() {
    useHook(); // Ok
    useHooklikeAnnotatedFunction(); // Error
    useDeclaredHooklikeFunction(); // Error
    useAssignedHooklikeArbitraryExpression(); // Error
    useAssignedHooklikeFunctionExpression(); // Error
    return 42;
}

function FunctionComponent(_props: unknown) {
    useHook(); // Error: definitely not component, because props is not subtype of `interface {}`
    useHooklikeAnnotatedFunction(); // Ok
    useDeclaredHooklikeFunction(); // Ok
    useAssignedHooklikeArbitraryExpression(); // Ok
    useAssignedHooklikeFunctionExpression(); // Ok
}

function notAFunctionComponent(_props: unknown) {
    useHook(); // Error
    useHooklikeAnnotatedFunction(); // Ok
    useDeclaredHooklikeFunction(); // Ok
    useAssignedHooklikeArbitraryExpression(); // Ok
    useAssignedHooklikeFunctionExpression(); // Ok
}
