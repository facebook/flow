declare function assertNumber(value: unknown): asserts value is number;
declare function assertTruth(value: unknown): asserts value;

module.exports = {assertNumber, assertTruth};
