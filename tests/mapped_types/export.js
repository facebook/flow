type O = {foo: number, bar?: string, +baz: boolean};

export type MappedO = {[key in keyof O]: O[key]};
export type AddOptional = {[key in keyof O]?: O[key]};
export type AllReadonly = {+[key in keyof O]: O[key]};

export type ParameterizedId<O: {...}> = {[key in keyof O]: O[key]};
export type ParameterizedPartial<O: {...}> = {[key in keyof O]?: O[key]};
export type ParameterizedReadonly<O: {...}> = {+[key in keyof O]: O[key]};

export type MappedNonHomomorphic = {[key in 'foo' | 'bar']: number};

export type SemiHomomorphic<O: {...}, Keys: keyof O> = {[key in Keys]: O[key]};
