// @flow strict-local

export {};

const element: HTMLDivElement = document.createElement('div');
const promise: Promise<HTMLDivElement> = Promise.resolve(element);
const divs = document.querySelectorAll('div');
const elements = document.querySelectorAll<Element>('*');
const htmlElements = document.querySelectorAll<HTMLElement>('*');
declare const index: number;
const firstElement: Element = elements[0];
const firstHTMLElement: HTMLElement = htmlElements[0];
const indexedElement: Element = elements[index];
const indexedHTMLElement: HTMLElement = htmlElements[index];
elements[index] as number; // ERROR
htmlElements[index] as number; // ERROR
const children = Array.from(document.body.children);
const formData = new FormData();

import.meta.url as string;
import.meta.resolve('example') as string;
element.align as string;
divs[0].align as string;
elements[0].attributes;
htmlElements[0].accessKey;
children[0].tagName as string;
formData.entries();
for (const div of divs) {
  div.align as string;
}
element.align as number; // ERROR
promise as Promise<string>; // ERROR
