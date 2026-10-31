import {Exported} from './optional_class_field_exported';

class Box {
  value?: number;
  #label?: string;
  static count?: number;

  readLabel(): string | void {
    return this.#label; // OK
  }

  readLabelAsString(): string {
    return this.#label; // ERROR
  }
}

function readPublic(box: Box) {
  box.value as number | void; // OK
  box.value as number; // ERROR
}

function writeUndefined(box: Box) {
  box.value = undefined; // OK
}

function readStatic() {
  Box.count as number | void; // OK
  Box.count as number; // ERROR
}

function readImported(exported: Exported) {
  exported.value as number | void; // OK
  exported.value as number; // ERROR
}

class WithInitializer {
  value?: number = 1;
}

function readInitialized(box: WithInitializer) {
  box.value as number; // ERROR
  box.value = undefined; // OK
}
