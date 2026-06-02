type O<out T> = { (x: T): void }; // Error: out T in a negative position
interface I<out T> { (x: T): void }; // Error: out T in a negative position
