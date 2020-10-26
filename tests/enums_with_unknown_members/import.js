// @flow

import E from './export.js';

const e: E = E.A;

switch (e) {
  case E.A: break;
  case E.B: break;
  default: break; // Not an error, because enum has unknown members
}
