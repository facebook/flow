const element: HTMLDivElement = document.createElement('div');
const promise: Promise<HTMLDivElement> = Promise.resolve(element);

import.meta.url as string;
import.meta.resolve('example') as string;
element.align as string;
element.align as number; // ERROR
promise as Promise<string>; // ERROR
