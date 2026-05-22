/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

interface TSLibMethodBivarianceEvent {
  kind: string;
}

interface TSLibMethodBivarianceDogEvent extends TSLibMethodBivarianceEvent {
  dog: string;
}

interface TSLibMethodBivarianceBaseEventMap {
  base: TSLibMethodBivarianceEvent;
}

interface TSLibMethodBivariancePreciseEventMap extends TSLibMethodBivarianceBaseEventMap {
  dog: TSLibMethodBivarianceDogEvent;
}

interface TSLibMethodBivarianceBaseTarget {
  addEventListener<K extends keyof TSLibMethodBivarianceBaseEventMap>(
    type: K,
    listener: (this: TSLibMethodBivarianceBaseTarget, ev: TSLibMethodBivarianceBaseEventMap[K]) => any,
  ): void;
  addEventListener(type: string, listener: (ev: TSLibMethodBivarianceEvent) => any): void;
}

interface TSLibMethodBivariancePreciseTarget extends TSLibMethodBivarianceBaseTarget {
  addEventListener<K extends keyof TSLibMethodBivariancePreciseEventMap>(
    type: K,
    listener: (
      this: TSLibMethodBivariancePreciseTarget,
      ev: TSLibMethodBivariancePreciseEventMap[K],
    ) => any,
  ): void;
  addEventListener(type: string, listener: (ev: TSLibMethodBivarianceEvent) => any): void;
}
