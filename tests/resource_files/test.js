import './foo.css'; // ok
import './foo'; // error, .css extension cannot be omitted

// By default, .css files export the Object type
require('./foo.css') as string; // ok, any ~> string

// active and hovered have the type any
import { active, hovered } from './foo.css';
active as string; // ok
hovered as number; // ok

import './bar.png';
import './bar'; // error, .png extension cannot be omitted
// By default, .png files export the string type
require('./bar.png') as number; // error, string ~> number

import './qux.webp'; // ok
import './qux'; // error, .webp extension cannot be omitted
require('./qux.webp') as number;  // error, string ~> number
