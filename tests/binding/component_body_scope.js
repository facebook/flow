// @flow

let featureEnabled = false;

type Mode = 'primary' | 'secondary';

declare var React: any;
declare component Toggle(disabled: boolean);
declare function resetRefinements(): void;

const primaryMode: Mode = 'primary';

component Options(disabled: boolean, mode: ?Mode) {
  featureEnabled = true;
  resetRefinements();
  return (
    <Toggle
      disabled={
        (mode === primaryMode && featureEnabled === false) || disabled
      }
    />
  );
}
