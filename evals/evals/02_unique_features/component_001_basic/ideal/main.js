/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import * as React from 'react';

function getAgeCategory(age: number): string {
  if (age < 13) {
    return 'Child';
  } else if (age < 20) {
    return 'Teen';
  } else if (age < 65) {
    return 'Adult';
  }
  return 'Senior';
}

function truncate(text: string, maxLength: number): string {
  if (text.length > maxLength) {
    return text.slice(0, maxLength) + '...';
  }
  return text;
}

export default component UserCard(
  name: string,
  age: number,
  bio: string = "No bio provided",
  isOnline: boolean = false,
) {
  return (
    <div>
      <h2>{name} ({getAgeCategory(age)})</h2>
      <p>Age: {age}</p>
      <p>{truncate(bio, 100)}</p>
      <span>{isOnline ? 'Online' : 'Offline'}</span>
    </div>
  );
}
