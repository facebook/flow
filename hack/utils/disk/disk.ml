module Stub = struct
  let cat = TestDisk.get
  let write_file ~file ~contents = TestDisk.set file contents
end

include (val (if Injector_config.use_test_stubbing
  then (module Stub : Disk_sig.S)
  else (module RealDisk : Disk_sig.S)
))
