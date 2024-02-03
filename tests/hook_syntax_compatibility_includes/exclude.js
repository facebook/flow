import * as IP from './include_provider';
import * as XP from './exclude_provider';

function FunctionComponent() {
    IP.useFoo(); // error
    IP.useBar(); // ok
    XP.useFoo(); // error
    XP.useBar(); // ok
}

component Component() {
    IP.useFoo(); // ok
    IP.useBar(); // error
    XP.useFoo(); // ok
    XP.useBar(); // error
    return null;
}
