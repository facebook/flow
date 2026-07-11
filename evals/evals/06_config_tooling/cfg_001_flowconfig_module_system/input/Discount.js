// @flow

export function applyDiscount(price: number, percentOff: number): number {
  return price - (price * percentOff) / 100;
}
