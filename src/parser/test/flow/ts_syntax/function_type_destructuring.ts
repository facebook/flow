type NavIcon = ({className}: {className?: string}) => ReactElement;
type ArrayFn = ([first, second]: [string, number]) => void;
type Nested = ({a: {b}, ...rest}: T) => R;
type Mixed = (x: T, {a}: T, [b]: T) => R;
type RestArray = (...[a, b]: T) => R;
type RestObject = (...{a}: T) => R;
type Generic = <T>({a}: {a: T}) => T;
type Optional = ({a}?: T) => R;
type OptionalArray = ([a]?: T) => R;
