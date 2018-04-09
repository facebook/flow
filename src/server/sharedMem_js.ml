include SharedMem

module Prefix = struct
  include Prefix
end

module Ident = struct
  include Ident
end

let collect effort =
  MonitorRPC.status_update ~event:ServerStatus.GC_start;
  SharedMem.collect effort
