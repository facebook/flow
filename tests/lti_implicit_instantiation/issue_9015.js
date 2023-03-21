// @flow

type ResultSet<Row> = {
  +rowCount: number,
  +rows: Array<Row>,
};

interface Submittable {
  submit: () => void;
}

declare class Client {
  constructor(): void;
  query<R>(config: string): Promise<ResultSet<R>>;
  query<Q: Submittable>(config: Q): Q;
}

declare var c: Client;

c.query('SELECT 1');
c.query({submit: () => undefined});
