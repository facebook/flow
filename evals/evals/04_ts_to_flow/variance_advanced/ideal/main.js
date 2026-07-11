/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type Animal = {readonly name: string, ...};
type Dog = {readonly name: string, readonly breed: string, ...};

class ReadonlyCell<out T> {
  readonly value: T;

  constructor(value: T) {
    this.value = value;
  }

  get(): T {
    return this.value;
  }
}

class Sink<in T> {
  readonly #handle: (item: T) => void;

  constructor(handle: (item: T) => void) {
    this.#handle = handle;
  }

  send(item: T): void {
    this.#handle(item);
  }
}

function nameOf(cell: ReadonlyCell<Animal>): string {
  return cell.get().name;
}

function adoptDog(sink: Sink<Dog>): void {
  sink.send({name: 'Rex', breed: 'Lab'});
}

const dogCell: ReadonlyCell<Dog> = new ReadonlyCell({name: 'Rex', breed: 'Lab'});

const animalSink: Sink<Animal> = new Sink((a: Animal) => {
  console.log(`Handling ${a.name}`);
});

console.log(nameOf(dogCell));
adoptDog(animalSink);
