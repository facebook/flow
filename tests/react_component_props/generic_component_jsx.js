import * as React from 'react';

declare component Inner<T extends {...}>(items: Array<T>);

declare component ViaElementConfig<T extends {...}>(
  ...props: React.ElementConfig<typeof Inner<T>>
);
declare component ViaComponentProps<T extends {...}>(
  ...props: React.ComponentProps<typeof Inner<T>>
);

declare const items: Array<{id: string}>;

component Test() {
  return (
    <>
      <ViaElementConfig items={items} /> {/* ok */}
      <ViaComponentProps items={items} /> {/* ok */}
    </>
  );
}
