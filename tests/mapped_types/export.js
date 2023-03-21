type O = {foo: number, bar?: string, +baz: bool};

export type MappedO = {[key in keyof O]: O[key]};
export type AddOptional = {[key in keyof O]?: O[key]};
export type AllReadonly = {+[key in keyof O]: O[key]};

export type ParameterizedId<O: {...}> = {[key in keyof O]: O[key]};
export type ParameterizedPartial<O: {...}> = {[key in keyof O]?: O[key]};
export type ParameterizedReadonly<O: {...}> = {+[key in keyof O]: O[key]};
