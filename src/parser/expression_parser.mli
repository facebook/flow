(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Expression
    (_ : Parser_common.PARSER)
    (_ : Parser_common.TYPE)
    (_ : Parser_common.DECLARATION)
    (_ : Parser_common.COVER) : Parser_common.EXPRESSION
