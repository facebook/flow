var M = require('M');
var N = require('N');
N.x = M(N.x);
var P = require('./P'); // implementation of P redirects to module M
N.x = P(N.x);
var Q = require('Q'); // declaration of Q redirects to module M
N.x = Q(N.x);
