// regression test for speculation-related panic

interface RequiredHeaders {
  constructor(): void;
  append(name: string, value: string): void;
  entries(): Iterator<[string, string]>;
  get(name: string): null | string;
  has(name: string): boolean;
}

interface ExpectedResponse {
  arrayBuffer(): Promise<ArrayBuffer>;
  headers: RequiredHeaders;
  ok: boolean;
  status: number;
  url: string;
}

function check(): Promise<void> {
  return (fetchLike() as Promise<ExpectedResponse>).then( // error
    (response: ExpectedResponse) => {
      response.headers.get('x');
      return response.arrayBuffer().then(() => {});
    },
  );
}

check();
