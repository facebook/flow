import {createContext, useContext} from 'react'

export opaque type Tag = number;

export const Context: React.Context<Tag> = createContext<Tag>(3);

component Foo() {
  const context = useContext(Context);
  context as number; // OK
  return null;
}
