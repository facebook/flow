// @flow

declare function first<T>(Array<T>): ?T;
first(1); // Incompatibility error, but no error on under-constrained T
