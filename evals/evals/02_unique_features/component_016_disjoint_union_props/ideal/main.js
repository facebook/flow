/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import * as React from 'react';

type ProductLine = {kind: 'product', name: string, qty: number, price: number};
type DiscountLine = {kind: 'discount', label: string, amount: number};
type LineProps = ProductLine | DiscountLine;

export component CartLine(...props: LineProps) {
  if (props.kind === 'product') {
    const total = props.qty * props.price;
    return <div>{props.name} × {props.qty} = {'$' + total.toFixed(2)}</div>;
  } else {
    return <div>{props.label}: −{'$' + props.amount.toFixed(2)}</div>;
  }
}

export component Receipt() {
  return (
    <div>
      <CartLine kind="product" name="Notebook" qty={3} price={4.5} />
      <CartLine kind="discount" label="Member discount" amount={2} />
    </div>
  );
}
