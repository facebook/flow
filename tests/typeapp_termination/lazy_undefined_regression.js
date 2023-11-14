// Regression test against CamlinternalLazy.Undefined exception when flattening
// OpenT FullyResolved in union_ident_map_and_dedup.

class Dialog {
  async show(): Promise<this> {
    await this.display();
    return this;
  }
  async display(): Promise<this> {
    return this;
  }
}
