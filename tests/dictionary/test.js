type Params = {count: number; [name: string]: string};
type QueryFunction = (params: Params) => string;

var o: { foo: QueryFunction } = {
  foo(params) {
    params.count = params.oops;
    return params.count;
  }
};

module.exports = o;
