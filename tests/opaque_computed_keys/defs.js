export opaque type Op: string = 'special';

export const op: Op = 'special';

const obj = {[op]: 'abc'};

obj as $ReadOnly<{[Op]: string}>;
obj as {[Op]: string};

obj as {[string]: string};
obj as {['special']: string};

obj as {['other']: string}; // error 'other' is incompatible with 'special'
