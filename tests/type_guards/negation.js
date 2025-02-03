function test0() {
  declare var isFstParamNum: (value: mixed, extra_value: mixed) => value is number;

  declare var x: number | string;
  declare var y: number | string;

  if (isFstParamNum(x, y)) {
    return;
  }
  x as string; // okay (all LatentP refinements are negated together)
}

function test1() {
  declare var obj: { check: (value: mixed) => value is number };

  declare var x: number | string;
  if (obj.check(x)) {
    x as number;
    return;
  }
  x as string; // okay (all LatentP/LatentThisPs refinements are negated together)
}
