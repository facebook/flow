/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Modifications Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use
 * this file except in compliance with the License. You may obtain a copy of the
 * License at http://www.apache.org/licenses/LICENSE-2.0
 * THIS CODE IS PROVIDED ON AN *AS IS* BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, EITHER EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION ANY IMPLIED
 * WARRANTIES OR CONDITIONS OF TITLE, FITNESS FOR A PARTICULAR PURPOSE,
 * MERCHANTABILITY OR NON-INFRINGEMENT.
 * See the Apache Version 2.0 License for specific language governing permissions
 * and limitations under the License.
 *
 * @flow
 */
// @lint-ignore-every LICENSELINT
interface String {
    /**
     * Returns a new String consisting of the single UTF-16 code unit located at
     * the specified offset. This method allows for positive and negative integers.
     * Negative integers count back from the last string character.
     * @param index The index (position) of the string character to be returned.
     * Supports relative indexing from the end of the string when passed a negative index;
     * i.e. if a negative number is used, the character returned will be found by
     * counting back from the end of the string.
     * @return A String consisting of the single UTF-16 code unit located at the specified position.
     * Returns undefined if the given index can not be found.
     */
    at(index: number): string | void;
}
