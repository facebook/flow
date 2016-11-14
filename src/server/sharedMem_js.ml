include SharedMem

module Prefix = struct
  include Prefix
end

module Ident = struct
  include Ident
end

(**
 * Wrap `collect` with a lock in order to communicate to the client when the
 * server is busy collecting.
 *)
let collect opts effort =
  let root = Options.root opts in
  let tmp_dir = Options.temp_dir opts in
  let lock_file = Server_files_js.gc_file ~tmp_dir root in
  ignore (Lock.grab lock_file);
  (try SharedMem.collect effort with exn ->
    prerr_endline
      "Exception thrown while collecting! Releasing lock then re-raising...";
    ignore (Lock.release lock_file);
    raise exn
  );
  ignore (Lock.release lock_file)
