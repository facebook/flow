/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

interface ProfileForm {
  name?: string;
  email?: string;
  age?: number;
}

type Submitted<T> = { [K in keyof T]-?: T[K] };

type Draft<T> = { [K in keyof T]+?: T[K] };

type Frozen<T> = { readonly [K in keyof T]: T[K] };

type Validation<T extends readonly unknown[]> = { [K in keyof T]: boolean };

type Fields = [string, string, number];

function submit(form: Submitted<ProfileForm>): string {
  return `${form.name} <${form.email}> (${form.age})`;
}

function freeze(form: Submitted<ProfileForm>): Frozen<ProfileForm> {
  return form;
}

const validity: Validation<Fields> = [true, true, false];
const draft: Draft<ProfileForm> = {};
const full: Submitted<ProfileForm> = {
  name: "Ada",
  email: "ada@x.io",
  age: 36,
};

console.log(submit(full), freeze(full), validity, draft);
