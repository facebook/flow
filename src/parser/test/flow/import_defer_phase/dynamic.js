const x = import.defer("module");
async function f() {
  return await import.defer("module", { with: { type: "json" } });
}
