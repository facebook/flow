interface Box<T> {
  value: T;
  get(): T;
}

function box(value: string): Box<string>;
function box(value: number): Box<number>;
function box(value: boolean): Box<boolean>;
function box<T>(value: T): Box<T> {
  return {
    value,
    get() {
      return value;
    },
  };
}

interface Theme {
  color: string;
  spacing: number;
  rounded: boolean;
}

function getToken(theme: Theme, key: "color"): string;
function getToken(theme: Theme, key: "spacing"): number;
function getToken(theme: Theme, key: "rounded"): boolean;
function getToken(theme: Theme, key: keyof Theme): string | number | boolean {
  return theme[key];
}

interface Identifiable {
  id: number;
}

function indexById<T extends Identifiable>(items: readonly T[]): Map<number, T> {
  const map = new Map<number, T>();
  for (const item of items) {
    map.set(item.id, item);
  }
  return map;
}

const theme: Theme = { color: "blue", spacing: 8, rounded: true };

const stringBox = box("hello");
const numberBox = box(42);
const color: string = getToken(theme, "color");
const spacing: number = getToken(theme, "spacing");

interface User extends Identifiable {
  name: string;
}
const users: User[] = [
  { id: 1, name: "Alice" },
  { id: 2, name: "Bob" },
];
const byId = indexById(users);

console.log(stringBox.get(), numberBox.get(), color, spacing, byId.get(1)?.name);
