import { useFoo } from './exp';

useFoo() as empty; // error

component U() {
    const x = useFoo();
    x[0] = 1; // error
    return null;
}
