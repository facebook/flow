// @flow

function callInNestedFunction(): void {}

class BooleanLiteralArrayFields {
  #enabled: boolean;

  constructor(enabled: boolean): void {
    this.#enabled = enabled;
  }

  getFields(xs: Array<string>): Array<any> {
    const fields = xs.map(() => {
      if (this.#enabled) {
        const nested = () => callInNestedFunction();
      }

      return {
        sortable: !this.#enabled,
      };
    });

    fields.unshift({
      sortable: !this.#enabled,
    });

    return fields;
  }
}
