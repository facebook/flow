// @flow

import type Route from './Route';
import type {JSResourceReference} from './JSResourceReference';
type Module<TDefaultExport> = {default: TDefaultExport, ...};

export type SurfaceEntryPoint<TRoute: Route<>> = $ReadOnly<{
//          ^
  getNavigationOptions: () => number,
  getRoute: () => Class<TRoute>,
  getComponent: () => JSResourceReference<Module<TRoute>>,
}>;
