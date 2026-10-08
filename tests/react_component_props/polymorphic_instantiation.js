import * as React from 'react';

type TPopover<P extends {...}> = component(...P);

declare component ContainerEC<T extends {...}>(
  f: (React.ElementConfig<TPopover<T>>) => void,
  z: T,
);
declare component ContainerCP<T extends {...}>(
  f: (React.ComponentProps<TPopover<T>>) => void,
  z: T,
);

type O = {x: number};

ContainerEC as TPopover<{f: (React.ElementConfig<TPopover<O>>) => void, z: O}>; // ok
ContainerCP as TPopover<{f: (React.ComponentProps<TPopover<O>>) => void, z: O}>; // ok

ContainerEC as TPopover<{f: (React.ElementConfig<TPopover<O>>) => void, z: {y: string}}>; // error: z conflicts with f
ContainerCP as TPopover<{f: (React.ComponentProps<TPopover<O>>) => void, z: {y: string}}>; // error: z conflicts with f

declare function g<T extends {...}>(
  f: (React.ComponentProps<TPopover<T>>) => void,
  z: T,
): void;
g as ((React.ComponentProps<TPopover<O>>) => void, O) => void; // ok
