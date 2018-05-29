include SharedMem

module Prefix = struct
  include Prefix
end

module Ident = struct
  include Ident
end

let collect effort =
  if SharedMem.should_collect effort
  then begin
    MonitorRPC.status_update ~event:ServerStatus.GC_start;
    SharedMem.collect effort
  end
