//@flow

declare function f<T> (x: number => T): T;
declare function f<T>(x: T => void): T;

f((x: string) => {}); // Ok: It will error under Pierce, but we also consider upper bounds here.

// --
declare function useCallback<T: (...args: $ReadOnlyArray<empty>) => mixed>(
  callback: T,
): T;

type Props = $ReadOnly<{
  fn: (...$ReadOnlyArray<empty>) => void,
}>;

function Component({
  fn,
}: Props) {
  const cb = useCallback(
    (...args) => {
        fn(...args);
    },
  );
  cb();
}
