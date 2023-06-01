function referenced_in_type_annotation_of_declare_var() {
  declare var x1: x1; // error recursive-definition
  declare var x2: typeof x2; // error recursive-definition
  declare var V1: $Call<<V1>(V1) => V1, number>; // okay
  declare var V2: $Call<<V2>(V2) => V2, V2>; // error recursive-definition

  declare var rec_obj_okay: { f: typeof rec_obj_okay }; // okay due to object type constructor
  declare var rec_obj_err:
    | { f: typeof rec_obj_err }
    | typeof rec_obj_err; // error here
}

function referenced_in_type_annotation_of_const() {
  const x1: x1 = 1; // error on x1 not `1`
  const x2: typeof x2 = 1; // error on x2 not `1`
  const x3: typeof x3.f = 1; // error on x2 not `1`
}

function recursion_limit_exceeded_regression_1() {
  declare var Val: Val<string>; // error on Val, no recursion-limit-exceeded
  const {f} = Val;
}

function recursion_limit_exceeded_regression_2() {
  let Val: Val<string>;
  Val = (42: any);
  const {f} = Val; // error on Val, no recursion-limit-exceeded
}
