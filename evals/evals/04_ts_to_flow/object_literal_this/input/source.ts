const flags = {
  enabled: {} as {[k: string]: boolean},
  changes: 0,

  set(name: string, on: boolean): boolean {
    const prev = this.enabled[name] ?? false;
    if (prev !== on) {
      this.enabled[name] = on;
      this.changes += 1;
    }
    return prev;
  },

  isOn(name: string): boolean {
    return this.enabled[name] ?? false;
  },

  activeCount(): number {
    let n = 0;
    for (const k of Object.keys(this.enabled)) {
      if (this.enabled[k]) {
        n += 1;
      }
    }
    return n;
  },
};

flags.set("new_ui", true);
flags.set("beta_search", true);
flags.set("beta_search", false);
console.log(`active=${flags.activeCount()} changes=${flags.changes} ui=${String(flags.isOn("new_ui"))}`);
