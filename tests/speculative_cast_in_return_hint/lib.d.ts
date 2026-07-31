interface Headers {
  append(name: string, value: string): void;
  get(name: string): string | null;
  has(name: string): boolean;
}

declare var Headers: {
  prototype: Headers;
  new(): Headers;
};

interface Response {
  arrayBuffer(): Promise<ArrayBuffer>;
  readonly headers: Headers;
  readonly ok: boolean;
  readonly status: number;
  readonly url: string;
}

declare function fetchLike(): Promise<Response>;
