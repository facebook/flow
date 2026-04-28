import type {F} from './cross_file_reverse_def';

declare const f: F;
f.x as number;
f.y as string;
f.x as string; // ERROR
f.y as number; // ERROR
