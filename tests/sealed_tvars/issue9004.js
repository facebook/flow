//@flow

function isRecursiveRate(rate: {+type: string, ...}): boolean %checks {
    return (
    rate.type === "recursive_constant" ||
    rate.type === "recursive_basis" ||
    rate.type === "recursive_sum" ||
    rate.type === "recursive_product" ||
    rate.type === "recursive_max" ||
    rate.type === "recursive_min" ||
    rate.type === "recursive_simple_break" ||
    rate.type === "recursive_choose"
  );
}

export const usesRecursiveBases = (
  rate: ?{+type: string, ...}
): boolean %checks => {
  return Boolean(
    rate &&
      (isRecursiveRate(rate) ||
        rate.type === "formula" ||
        // $FlowFixMe[incompatible-type]
        rate.type === "break")
  );
}
