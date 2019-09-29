include ( val if Injector_config.use_test_stubbing then
                (module TestDisk : Disk_sig.S)
              else
                (module RealDisk : Disk_sig.S) )
