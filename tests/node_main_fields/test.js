import A from 'A'; // This module has a foo property
import B from 'B'; // This module has a bar property
import C from 'C'; // Error - this module has neither a foo nor bar property

A as empty; // Error: string ~> empty
B as empty; // Error: boolean ~> empty