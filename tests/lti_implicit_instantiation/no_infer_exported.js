declare export function f1<T>(v1: T, v2: T): void;
declare export function f2<T>(v1: T, v2: NoInfer<T>): void;
declare export function f3<T>(v1: T, v2: {f: T}): void;
declare export function f4<T>(v1: T, v2: NoInfer<{f: T}>): void;
