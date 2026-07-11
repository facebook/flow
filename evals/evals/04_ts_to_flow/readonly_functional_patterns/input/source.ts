type Product = Readonly<{
  id: number;
  name: string;
  price: number;
  tags: readonly string[];
}>;

function totalPrice(products: readonly Product[]): number {
  return products.reduce((sum, p) => sum + p.price, 0);
}

function applyDiscount(
  products: readonly Product[],
  pct: number
): readonly Product[] {
  return products.map((p) => ({ ...p, price: p.price * (1 - pct) }));
}

function allTags(products: readonly Product[]): readonly string[] {
  const seen = new Set<string>();
  for (const p of products) {
    for (const tag of p.tags) {
      seen.add(tag);
    }
  }
  return [...seen];
}

const catalog: readonly Product[] = [
  { id: 1, name: "Mug", price: 10, tags: ["kitchen", "ceramic"] },
  { id: 2, name: "Notebook", price: 5, tags: ["office"] },
];

const discounted = applyDiscount(catalog, 0.1);

console.log(totalPrice(catalog), totalPrice(discounted), allTags(catalog));
