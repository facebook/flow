import f1 from './file1';
f1("") as string;
f1(0) as number;

import f2 from './file2';
f2("") as string;
f2(0) as number;

import f3 from './file3';
f3("") as string;
f3(0) as number;
f3(true) as boolean;

import c from './file4';
(new c).x as string;
(new c).x as number;

import f5 from './file5';
f5 as number;
f5() as number;
