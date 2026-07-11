interface Address {
  city: string;
  zip?: string;
}

interface User {
  name: string;
  address?: Address | null;
}

function getZip(user: User | null | undefined): string {
  return user?.address?.zip ?? "00000";
}

function formatUser(user: User | null): string {
  if (user == null) {
    return "anonymous";
  }
  const city = user.address?.city ?? "unknown";
  return `${user.name} (${city})`;
}

class Cache<V> {
  private store = new Map<string, V>();

  get(key: string): V | undefined {
    return this.store.get(key);
  }

  getOrDefault(key: string, fallback: V): V {
    const value = this.store.get(key);
    return value !== undefined ? value : fallback;
  }

  set(key: string, value: V): void {
    this.store.set(key, value);
  }
}

const user: User = { name: "Alice", address: { city: "NYC" } };
const cache = new Cache<number>();
cache.set("a", 1);

console.log(getZip(user), formatUser(null), cache.getOrDefault("b", 0));
