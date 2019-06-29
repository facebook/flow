// @flow

declare function onAction<T, Result>(
  myEnum: T,
  // TODO: bug with ExactT ?
  pattern: $Exact<$ObjSingleton<T, () => Result>>
): Result;

declare var action: "Action1" | "Action2" | "Action3";

// no error
const result: string = onAction(action, {
  Action1: () => "",
  Action2: () => "",
  Action3: () => ""
});

// error
const result2: number = onAction(action, {
  Action1: () => 0,
  Action2: () => 1
});
