/* @flow */

var os = require('os');

var u1 = os.userInfo();
u1.username as string;
u1.username as Buffer; // error

var u2 = os.userInfo({encoding: 'utf8'});
u2.username as string;
u2.username as Buffer; // error

var u3 = os.userInfo({encoding: 'buffer'});
u3.username as string; // error
u3.username as Buffer;
