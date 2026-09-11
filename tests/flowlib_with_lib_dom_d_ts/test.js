const element: HTMLDivElement = document.createElement('div');
const promise: Promise<HTMLDivElement> = Promise.resolve(element);

element.align as string;
element.align as number; // ERROR
promise as Promise<string>; // ERROR
