/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type Form = {
  email: string,
  password: string,
};

type FieldErrors = $ObjMapConst<Form, ?string>;

export function firstError(errors: FieldErrors): ?string {
  return errors.email ?? errors.password;
}
