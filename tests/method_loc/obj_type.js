declare var o: { m(): void };
o.m as empty; // err
module.exports = o;
