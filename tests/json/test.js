// @flow

import a1 from './a';       // okay, imported as resource
import a2 from 'a';         // cannot resolve module
import b1 from './b.css';   // okay, resource
import b2 from './b';       // cannot resolve without extension
import c1 from 'c';         // cannot resolve module
import p1 from './package'; // okay, imported as resource
import p2 from 'package';   // cannot resolve module

(a1: empty); // okay
(a2: empty); // okay
(b2: empty); // okay
(b2: empty); // okay
(c1: empty); // okay
(p1: empty); // okay
(p2: empty); // okay
