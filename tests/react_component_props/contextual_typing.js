import * as React from 'react';

declare component Inner<T>(onChange: (string) => void, value?: T);

declare component ViaElementConfig<T>(
  ...props: React.ElementConfig<typeof Inner<T>>
);
declare component ViaComponentProps<T>(
  ...props: React.ComponentProps<typeof Inner<T>>
);

component Test() {
  return (
    <>
      <ViaElementConfig onChange={v => {}} /> {/* ok */}
      <ViaComponentProps onChange={v => {}} /> {/* ok */}
    </>
  );
}
