Write a Flow function `offset(q: Quadrant, distance: number): string` that uses a single `match` expression to look up a quadrant's x and y direction (each `-1` or `1`), then returns `"dx=<x>, dy=<y>"` where each direction is multiplied by `distance`.

The quadrants are `'NE'`, `'NW'`, `'SE'`, `'SW'`. For example `'NE'` is right and up: x direction `1`, y direction `-1`. Get both directions out of the one `match`.
