/* @flow */

type NoSpaces = 'foobar';
'foobar' as NoSpaces;

type HasSpaces = 'foo bar';
'foo bar' as HasSpaces;
