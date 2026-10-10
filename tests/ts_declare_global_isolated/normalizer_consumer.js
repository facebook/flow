import type {Imported} from "./normalizer";

declare const stream: GlobalSymbols.Stream;
stream as empty; // ERROR

declare const imported: Imported;
imported as empty; // ERROR
