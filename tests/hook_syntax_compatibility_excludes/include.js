import * as IP from './include_provider';
import * as XP from './exclude_provider';

function FunctionComponent() {
    IP.useFoo(); // ok
    IP.useBar(); // ok
    XP.useFoo(); // ok
    XP.useBar(); // ok
}

component Component() {
    IP.useFoo(); // ok
    IP.useBar(); // ok
    XP.useFoo(); // ok
    XP.useBar(); // ok
    return null;
}
