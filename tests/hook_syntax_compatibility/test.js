const useZ = require('./exp');

const useX = 42;

import { useW } from './exp2';
import useU from './exp2';
import * as useV from './exp2';

component C(...{ useY }: { useY: () => void}) {
    useY(); // no error
    useX(); // error for calling number but no hook error
    useZ(); // no error
    useW(); // no error
    useU(); // no error
    useV(); // error for calling module but no hook error
    return 42;
}
