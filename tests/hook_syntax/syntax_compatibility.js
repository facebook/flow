// Compatibility mode for rollout: restricts rules on hooklike functions and function components

export hook useHook() { }

export function useHooklikeAnnotatedFunction(): void { }

function useHooklikeInferredFunction() { return 42; }

declare export function useDeclaredHooklikeFunction(): number;

export const useAssignedHooklikeArbitraryExpression = (() => 42) as () => number;

export const useAssignedHooklikeFunctionExpression = (): number => 42;

useHook(); // Error
useHooklikeAnnotatedFunction(); // Ok
useHooklikeInferredFunction(); // Ok
useDeclaredHooklikeFunction(); // Ok
useAssignedHooklikeArbitraryExpression(); // Ok
useAssignedHooklikeFunctionExpression(); // Ok

component C() {
    useHook(); // Ok
    useHooklikeAnnotatedFunction(); // Error
    useHooklikeInferredFunction(); // Error
    useDeclaredHooklikeFunction(); // Error
    useAssignedHooklikeArbitraryExpression(); // Error
    useAssignedHooklikeFunctionExpression(); // Error
    return 42;
}

function FunctionComponent(_props: mixed) {
    useHook(); // Error
    useHooklikeAnnotatedFunction(); // Ok
    useHooklikeInferredFunction(); // Ok
    useDeclaredHooklikeFunction(); // Ok
    useAssignedHooklikeArbitraryExpression(); // Ok
    useAssignedHooklikeFunctionExpression(); // Ok
}

function notAFunctionComponent(_props: mixed) {
    useHook(); // Error
    useHooklikeAnnotatedFunction(); // Ok
    useHooklikeInferredFunction(); // Ok
    useDeclaredHooklikeFunction(); // Ok
    useAssignedHooklikeArbitraryExpression(); // Ok
    useAssignedHooklikeFunctionExpression(); // Ok
}
