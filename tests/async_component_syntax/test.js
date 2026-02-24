// @flow
import * as React from 'react';

// Basic async component with await
async component AsyncBasic() {
  await Promise.resolve();
  return <div />;
}

// Async component can be used in JSX just like sync components
const _usage1 = <AsyncBasic />; // OK

// Async component with props
async component AsyncWithProps(foo: number, bar: string) {
  const _result = await Promise.resolve(foo);
  return <div>{bar}</div>;
}

const _usage2 = <AsyncWithProps foo={42} bar="hello" />; // OK
const _usage3 = <AsyncWithProps foo="bad" bar={42} />; // ERROR 2x

// Async component with renders annotation
async component AsyncWithRenders() renders AsyncBasic {
  await Promise.resolve();
  return <AsyncBasic />;
}

// Async component with type params
async component AsyncGeneric<T: string>(value: T) {
  const _result = await Promise.resolve(value);
  return <div />;
}

// Export async component
export async component ExportedAsync() {
  return <div />;
}

// Export default async component
export default async component DefaultAsync() {
  return <div />;
}

// Basic async hook with await
async hook useAsyncHook(): string {
  const result = await Promise.resolve("hello");
  return result;
}

// Async hook with params
async hook useAsyncHookWithParams(foo: number): Promise<number> {
  const result = await Promise.resolve(foo);
  return result;
}

// Async hook with type params
async hook useAsyncGenericHook<T: string>(value: T): Promise<T> {
  const result = await Promise.resolve(value);
  return result;
}

// Export async hook
export async hook useExportedAsyncHook(): string {
  return await Promise.resolve("hello");
}
