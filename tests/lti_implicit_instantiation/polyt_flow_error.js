declare function first<T>(x: Array<T>): ?T;
first(1); // Incompatibility error, but no error on under-constrained T
