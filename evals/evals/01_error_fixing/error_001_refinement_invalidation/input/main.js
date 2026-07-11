/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 */

type User = {
  name: string,
  email: string,
  role: 'admin' | 'user' | 'guest',
};

class UserService {
  user: ?User;

  constructor(user: ?User) {
    this.user = user;
  }

  logAccess(): void {
    // side effect: could modify this.user
  }

  getUserDisplayName(): string {
    if (this.user != null) {
      this.logAccess();
      return this.user.name + ' (' + this.user.role + ')';
    }
    return 'Anonymous';
  }
}

export { UserService };
