import * as React from 'react';

export component MyExportedInput(other: string, ref: React$RefSetter<?HTMLElement>) {
    return <input id={other} ref={ref} />
}
