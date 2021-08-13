// @flow

export type TQuery<
  Data,
  Vars,
  Parent = TQuery<mixed, mixed, mixed>
//         ^ --max-depth 100000 --expand-type-aliases
> = {
  parent: Parent,
  vars: Vars,
  data: Data,
}
