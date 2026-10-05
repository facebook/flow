import * as React from 'react';

declare component Inner<T extends {...}>(items: Array<T>);

declare component WrapperEC<W extends {...}>(
  data: ReadonlyArray<W>,
  ...props: React.ElementConfig<typeof Inner<W>>
);
declare component WrapperCP<W extends {...}>(
  data: ReadonlyArray<W>,
  ...props: React.ComponentProps<typeof Inner<W>>
);

declare const items: Array<{id: string}>;

component Concrete() {
  return (
    <>
      <WrapperEC items={items} data={items} /> {/* ok */}
      <WrapperCP items={items} data={items} /> {/* ok */}
    </>
  );
}

component ForwardEC<U extends {...}>(
  data: ReadonlyArray<U>,
  ...props: React.ElementConfig<typeof Inner<U>>
) {
  return <WrapperEC {...props} data={data} />; // ok
}

component ForwardCP<U extends {...}>(
  data: ReadonlyArray<U>,
  ...props: React.ComponentProps<typeof Inner<U>>
) {
  return <WrapperCP {...props} data={data} />; // ok
}
