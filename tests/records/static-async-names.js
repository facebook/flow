// Properties named `static` and `async`
{
  record R {
    static: string,
    async: number,
  }
  declare const x: R;

  x.static as string; // OK
  x.static as empty; // ERROR: `string`

  x.async as number; // OK
  x.async as empty; // ERROR: `number`
}

// Methods named `static` and `async`
{
  record R {
    static(): number {
      return 0;
    }
    async(): string {
      return "test";
    }
  }
  declare const x: R;

  x.static() as number; // OK
  x.static() as empty; // ERROR: `number`

  x.async() as string; // OK
  x.async() as empty; // ERROR: `string`
}

// Static property named `async`
{
  record R {
    static async: number = 1,
  }

  R.async as number; // OK
  R.async as empty; // ERROR: `number`
}

// Static method named `async`
{
  record R {
    static async(): boolean {
      return true;
    }
  }

  R.async() as boolean; // OK
  R.async() as empty; // ERROR: `boolean`
}
