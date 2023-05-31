function referenced_in_type_annotation_of_func_param() {
  function okay_01(V: $Call<<V>(V) => V, number>): void {} // okay
  function okay_02(outer: number, {outer: y}: { outer: typeof outer }) {} // okay
  function okay_03(x: (x: mixed) => void) {} // okay: x as nested param
  function okay_rest(x: number, ...rest: Array<typeof x>) {} // okay

  function invalid_01(x: x): void {} // error on x
  function invalid_02(x: typeof x): void {} // error on x
  function invalid_03(V: $Call<<V>(V) => V, V>): void {} // error on V
  function invalid_04({x}: x) {} // error on x
  function invalid_05({x}: typeof x) {} // error on x
  function invalid_06({x:y}: typeof y) {} // error on y
  function invalid_07({x:[{z}]}: typeof z) {} // error on z

  function invalid_circular(x: typeof y, y: typeof x) {} // error cannot resolve 'y'
  function invalid_forward_with_default(
    x: typeof y /* error: cannot resolve 'y' */ = y /* error reference-before-declaration */,
    y: typeof x,
  ) {}
}

function recursion_limit_exceeded_regression_param<K>(
  Val: Val<K>, // error on Val, no recursion-limit-exceeded
): void {
  const {f} = Val;
}

function recursion_limit_exceeded_regression_rest_param<K>(
  ...Val: Val<K> // error on Val, no recursion-limit-exceeded
): void {
  const {f} = Val;
}

function recursion_limit_exceeded_regression_multi_param() {
  type getString<P> = (params: P) => string;

  const getString_ = <P>(
    getString: getString<P>,
    queryParams: P,
  ): string => {
    return getString({...queryParams});
  };
}

function body_bindings_not_visible_from_param_scope() {
  function foo(a: A) { // error cannot-resolve-name
    const A: number = 1;
  }
}
