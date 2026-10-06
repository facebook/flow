import * as React from 'react';

declare const ref: {current: DivInstance | null};

({ref}) as React.ElementConfig<'div'>; // ok
({ref}) as React.ComponentProps<'div'>; // ok
