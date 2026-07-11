/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type Product = Readonly<{
  id: number,
  name: string,
  price: number,
  tags: ReadonlyArray<string>,
}>;

function totalPrice(products: ReadonlyArray<Product>): number {
  return products.reduce((sum, p) => sum + p.price, 0);
}

function applyDiscount(
  products: ReadonlyArray<Product>,
  pct: number,
): ReadonlyArray<Product> {
  return products.map((p) => ({...p, price: p.price * (1 - pct)}));
}

function allTags(products: ReadonlyArray<Product>): ReadonlyArray<string> {
  const seen = new Set<string>();
  for (const p of products) {
    for (const tag of p.tags) {
      seen.add(tag);
    }
  }
  return [...seen];
}

const catalog: ReadonlyArray<Product> = [
  {id: 1, name: 'Mug', price: 10, tags: ['kitchen', 'ceramic']},
  {id: 2, name: 'Notebook', price: 5, tags: ['office']},
];

const discounted = applyDiscount(catalog, 0.1);

console.log(totalPrice(catalog), totalPrice(discounted), allTags(catalog));
