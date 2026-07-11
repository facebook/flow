/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import * as React from 'react';

component ProductCard(name: string, price: number) {
  return (
    <div>
      <h3>{name}</h3>
      <p>{'$' + price.toFixed(2)}</p>
    </div>
  );
}

component ProductGrid(products: Array<{id: string, name: string, price: number}>) {
  return (
    <div>
      {products.map(p => <ProductCard key={p.id} name={p.name} price={p.price} />)}
    </div>
  );
}
