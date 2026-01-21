import { useHook, useHooklikeAnnotatedFunction, useDeclaredHooklikeFunction, useAssignedHooklikeArbitraryExpression, useAssignedHooklikeFunctionExpression} from "./syntax_compatibility"

useHook(); // Error
useHooklikeAnnotatedFunction(); // Error: it's assumed to be hook
useDeclaredHooklikeFunction(); // Error: it's assumed to be hook
useAssignedHooklikeArbitraryExpression(); // Ok
useAssignedHooklikeFunctionExpression(); // Ok

component C() {
    useHook(); // Ok
    useHooklikeAnnotatedFunction(); // Ok
    useDeclaredHooklikeFunction(); // Ok
    useAssignedHooklikeArbitraryExpression(); // Ok
    useAssignedHooklikeFunctionExpression(); // Ok
    return 42;
}

function FunctionComponent(_props: mixed) {
    useHook(); // Ok
    useHooklikeAnnotatedFunction(); // Ok
    useDeclaredHooklikeFunction(); // Ok
    useAssignedHooklikeArbitraryExpression(); // Ok
    useAssignedHooklikeFunctionExpression(); // Ok
}

function notAFunctionComponent(_props: mixed) {
    useHook(); // Error
    useHooklikeAnnotatedFunction(); // Error: it's assumed to be hook
    useDeclaredHooklikeFunction(); // Error: it's assumed to be hook
    useAssignedHooklikeArbitraryExpression(); // Ok
    useAssignedHooklikeFunctionExpression(); // Ok
}
