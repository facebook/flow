declare export hook useRef<T>(initialValue: T): React$RefObject<T>;

declare type MaybeCleanUpFn = void | (() => void);
declare export hook useEffect(
    create: () => MaybeCleanUpFn,
    inputs?: ?ReadonlyArray<unknown>,
  ): void;
