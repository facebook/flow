// @flow

const { inexact } = require('./object_freeze');

(inexact: {||}); // error: inexact -> exact

inexact.p = 0; // error: can't set prop on frozen object
