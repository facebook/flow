declare var x: mixed;

function basic() {
  declare function f<V>(x: V): boolean %checks(typeof x === "number");

  if(f(x)) {
    (x: number);
    (x: string); // error
  }

  if(f<mixed>(x)) {
    (x: number);
    (x: string); // error
  }
}

function nested() {
  declare function f<V>(x: V): boolean %checks(typeof x === "number");
  declare function g(x: mixed): boolean %checks(f(x));

  if(g(x)) {
    (x: number);
    (x: string); // error
  }
}

function typeapp_expansion_underconstrained() {
  type O<K, V> = { k: K, v: V };

  declare function fromEntries<K, V>(entries: O<K, V>): { [K]: V, ... };
  declare var config: O<string, O<string, string>>;
  // This call would cause an [underconstrained-implicit-instantiation] error if
  // the typeapp expansion stack is not emptied before handling CallLatentPredT.
  if (fromEntries(config)) {
    (config: mixed);
  }
}

function underconstrained() {
  declare function fromMaybe<T>(x: mixed, ?{ f: T }): T;
  // Ensures the [underconstrained-implicit-instantiation] errors match between
  // CallT and CallLatentPredT
  if (fromMaybe(x)) { // a single [underconstrained-implicit-instantiation] error
    (x: mixed);
  }
}

function poly_id() {
  function id<X>(x: X): X %checks {
    return x;
  }
  declare var maybeObj: ?{};
  if (id(maybeObj)) {
    (maybeObj: {}); // okay
  } else {
    (maybeObj: {}); // error null or void ~> {}
  }
}

function poly_eval() {
  function truthyStr(x: string): $Call<<V>(V) => V, string> %checks {
    return x;
  }
  declare var str: string;
  if (!truthyStr(str)) {
    (str: ''); // okay
  }
}
