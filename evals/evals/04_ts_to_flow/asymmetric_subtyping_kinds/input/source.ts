interface RemoteQuote {
  weightKg: number;
  distanceKm: number;
  express: boolean;
}

class OrderQuote {
  weightKg: number;
  distanceKm: number;
  express: boolean;

  constructor(weightKg: number, distanceKm: number, express: boolean) {
    this.weightKg = weightKg;
    this.distanceKm = distanceKm;
    this.express = express;
  }
}

function computeShipping(quote: {
  weightKg: number;
  distanceKm: number;
  express: boolean;
}): number {
  const base = 2.5 + quote.weightKg * 1.75 + quote.distanceKm * 0.12;
  return quote.express ? base * 1.6 : base;
}

declare const providerQuote: RemoteQuote;

const literalQuote = {weightKg: 0.8, distanceKm: 15, express: false};
const orderQuote = new OrderQuote(3.2, 240, true);

const total =
  computeShipping(literalQuote) +
  computeShipping(orderQuote) +
  computeShipping(providerQuote);

console.log(`total shipping: ${total.toFixed(2)}`);
