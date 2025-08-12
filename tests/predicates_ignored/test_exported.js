import {exported_declare, exported_non_declare} from './exported';

declare const v: string | null;
if (exported_declare(v)) {
  v as string; // error: %checks is ignored
}
if (exported_non_declare(v)) {
  v as string; // error: %checks is ignored
}
