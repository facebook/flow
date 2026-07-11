/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 */

class Account {
  private balance: number = 0;
  protected overdraftLimit: number = 100;

  deposit(amount: number): void {
    if (amount <= 0) {
      return;
    }
    this.balance += amount;
  }

  withdraw(amount: number): boolean {
    if (amount <= 0) {
      return false;
    }
    if (this.balance - amount < -this.overdraftLimit) {
      return false;
    }
    this.balance -= amount;
    return true;
  }

  statement(): string {
    const sign = this.balance < 0 ? '-' : '';
    const abs = Math.abs(this.balance);
    return `${sign}$${abs.toFixed(2)}`;
  }
}

const a: Account = new Account();
a.deposit(250);
const ok: boolean = a.withdraw(80);
console.log(`${a.statement()} (last withdrawal ok: ${String(ok)})`);
