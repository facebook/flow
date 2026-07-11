type FormField =
  | { kind: "text"; value: string }
  | { kind: "number"; value: number };

function serialize(field: FormField): string {
  const { kind, value } = field;
  if (kind === "text") {
    return `text:${value.trim()}`;
  }
  return `number:${value.toFixed(2)}`;
}

const fields: FormField[] = [
  { kind: "text", value: "  hello  " },
  { kind: "number", value: 3.14159 },
];

for (const field of fields) {
  console.log(serialize(field));
}
