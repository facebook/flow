import * as React from 'react';

type TPopover<P extends {...}> = component(...P);

declare component ContainerCP<T extends {...}>(
  f: (React.ComponentProps<TPopover<T>>) => void,
  z: T,
);

type O = {x: number};

ContainerCP as TPopover<{f: (React.ComponentProps<TPopover<O>>) => void, z: O}>; // ok

ContainerCP as TPopover<{f: (React.ComponentProps<TPopover<O>>) => void, z: {y: string}}>; // error: z conflicts with f

declare function g<T extends {...}>(
  f: (React.ComponentProps<TPopover<T>>) => void,
  z: T,
): void;
g as ((React.ComponentProps<TPopover<O>>) => void, O) => void; // ok
