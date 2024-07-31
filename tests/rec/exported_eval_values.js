
declare const options: {[string]: Value};

export opaque type Value: number = $Values< // error: cyclic
  typeof options,
>;

export default options;
