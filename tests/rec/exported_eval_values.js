
declare const options: {[string]: Value};

export opaque type Value: number = $Values< // todo: internal error
  typeof options,
>;

export default options;
