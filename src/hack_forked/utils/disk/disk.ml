(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include ( val if Injector_config.use_test_stubbing then
                (module TestDisk : Disk_sig.S)
              else
                (module RealDisk : Disk_sig.S) )
