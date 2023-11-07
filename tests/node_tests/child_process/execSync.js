/* @flow */

var execSync = require('child_process').execSync;

execSync('ls') as Buffer; // returns Buffer
execSync('ls', {encoding: 'buffer'}) as Buffer; // returns Buffer
execSync('ls', {encoding: 'utf8'}) as string; // returns string
execSync('ls', {timeout: '250'}); // error, no signatures match
execSync('ls', {stdio: 'inherit'}); // error, no signatures match
execSync('ls', {stdio: ['inherit']}); // error, no signatures match
