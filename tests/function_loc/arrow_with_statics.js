const f = () => {};
f.x = 1;
f as empty; // err
f.x as empty; // err
module.exports = f;
