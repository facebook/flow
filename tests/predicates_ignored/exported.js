function local(x: string | null): %checks { return x !== null; } // error: unsupported
declare const v: string | null;
if (local(v)) {
  v as string; // error: %checks is ignored
}

export function exported_non_declare(x: string | null): boolean %checks { return x !== null; } // error: unsupported
declare export function exported_declare(x: string | null): boolean %checks(x !== null) // error: unsupported
