// @flow

import type Route from './Route';
import type {JSResourceReference} from './JSResourceReference';
type Module<TDefaultExport> = {default: TDefaultExport, ...};

export type SurfaceEntryPoint<TRoute extends Route<>> = Readonly<{
//          ^
  getNavigationOptions: () => number,
  getRoute: () => Class<TRoute>,
  getComponent: () => JSResourceReference<Module<TRoute>>,
 ...}>;
