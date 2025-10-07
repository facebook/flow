import { config } from "./config";

config as {n: number | string}; // error: n is not read-only in {n: number | string}
