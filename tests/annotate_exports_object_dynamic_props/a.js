// @flow

function keyA() { return 'keyA' as const };
function keyB() { return 'keyB' as const };
function keyAny(): any { return 'keyAny' };

const FIELDS = {
  A: keyA(),
  B: keyB(),
  keyAny: keyAny(),
}

const dict = {
  [FIELDS.A]: 1,
  [FIELDS.B]: 2,
  [FIELDS.keyAny]: 3,
};

module.exports = dict;
