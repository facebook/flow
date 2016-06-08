/* @flow */

(`foo`: string); // ok
(`bar`: 'bar'); // ok
(`baz`: number); // error

`foo ${123} bar`; // ok, number can be appended to string
`foo ${{bar: 123}} baz`; // error, object can't be appended
