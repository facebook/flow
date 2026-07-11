export interface Money {
  amount: number;
  currency: string;
}

export function add(a: Money, b: Money): Money {
  if (a.currency !== b.currency) {
    throw new Error("currency mismatch");
  }
  return { amount: a.amount + b.amount, currency: a.currency };
}

export function format(money: Money): string {
  return `${money.amount.toFixed(2)} ${money.currency}`;
}

const wallet: Money[] = [
  { amount: 10, currency: "USD" },
  { amount: 5.5, currency: "USD" },
  { amount: 2.25, currency: "USD" },
];

const total = wallet.reduce((acc, m) => add(acc, m), {
  amount: 0,
  currency: "USD",
});

console.log(format(total));
