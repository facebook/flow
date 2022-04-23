// @flow

var x = 42;
x++;
(++x: empty); // err
(x: empty); // err
x += 42;
(x: empty); // err
x -= 42;
(x: empty); // err
