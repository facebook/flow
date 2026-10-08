import * as React from 'react';

declare component Inner<T extends {...}>(items: Array<T>);

declare component ViaComponentProps<T extends {...}>(
  ...props: React.ComponentProps<typeof Inner<T>>
);

declare const items: Array<{id: string}>;

component Test() {
  return (
    <>
      <ViaComponentProps items={items} /> {/* ok */}
    </>
  );
}
