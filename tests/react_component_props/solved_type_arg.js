import * as React from 'react';

declare component Inner<T extends {...}>(items: Array<T>);

declare component WrapperCP<W extends {...}>(
  data: ReadonlyArray<W>,
  ...props: React.ComponentProps<typeof Inner<W>>
);

declare const items: Array<{id: string}>;

component Concrete() {
  return (
    <>
      <WrapperCP items={items} data={items} /> {/* ok */}
    </>
  );
}

component ForwardCP<U extends {...}>(
  data: ReadonlyArray<U>,
  ...props: React.ComponentProps<typeof Inner<U>>
) {
  return <WrapperCP {...props} data={data} />; // ok
}
