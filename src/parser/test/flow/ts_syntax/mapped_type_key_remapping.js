type T1 = {[K in keyof T as string]: T[K]};
type T2 = {[K in keyof T as T[K] extends never ? never : K]: T[K]};
type T3 = {[N in Node as N["type"]]?: (keyof N)[]};
type T4 = {[P in keyof T as T[P] extends U ? P : never]: T[P]};
