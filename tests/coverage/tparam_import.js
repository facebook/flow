// @flow

import { type RecordOf } from "./tparam";

export type Point = {
  x: number,
  y: number,
};

export type PointRecord = RecordOf<Point>;
