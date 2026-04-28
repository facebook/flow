import type {D, E} from './cross_file_def';

declare const d: D;
d.x as number;
d.y as string;
d.x as string; // ERROR
d.y as number; // ERROR

declare const e: E;
e.x as number;
e.y as string;
e.x as string; // ERROR
e.y as number; // ERROR
