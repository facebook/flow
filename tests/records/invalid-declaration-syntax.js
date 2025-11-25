// Semicolon suffix: single
{
  record R {
    a: number;
  }
}
{
  record R {
    static a: number = 1;
  }
}

// Semicolon suffix: multiple
{
  record R {
    a: number;
    static b: string = 's';
  }
}

// Variance: single
{
  record R {
    +a: number, // ERROR
  }
}

// Variance: multiple
{
  record R {
    +a: number, // ERROR
    static +b: string = 's', // ERROR
  }
}

// Optional: single
{
  record R {
    a?: number, // ERROR
  }
}

// Optional: multiple
{
  record R {
    a?: number, // ERROR
    b?: string, // ERROR
  }
}

// Multiple different errors
{
  record R {
    +a?: number; // ERROR
    +b?: string; // ERROR
  }
}
