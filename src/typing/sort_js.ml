
open Utils

(*******************************************)
(* topological sort of module dependencies *)
(*******************************************)

type topsort_state = {
  mutable not_yet_visited: (string * SSet.t) SMap.t;
  mutable cycle: bool;

  mutable stack: string list;
  mutable count: int;
  mutable visit_order: int SMap.t;
  mutable links: int SMap.t;
  mutable heights: int SMap.t;
}

let tsort = {
  not_yet_visited = SMap.empty;
  cycle = false;

  stack = [];
  count = 0;
  visit_order = SMap.empty;
  links = SMap.empty;
  heights = SMap.empty;
}

(* See:
   http://en.wikipedia.org/wiki/Tarjan's_strongly_connected_components_algorithm
*)
let rec tarjan () =
  try
    let (m,_) = SMap.choose tsort.not_yet_visited in
    strongconnected m |> ignore;
    tarjan ()
  with _ ->
    ()

and strongconnected m =
  tsort.visit_order <- tsort.visit_order |> SMap.add m tsort.count;
  let (_,local_requires) = SMap.find_unsafe m tsort.not_yet_visited in
  tsort.not_yet_visited <- SMap.remove m tsort.not_yet_visited;
  tsort.links <- tsort.links |> SMap.add m tsort.count;
  tsort.count <- tsort.count + 1;
  tsort.stack <- m :: tsort.stack;

  let height = ref 0 in
  local_requires |> SSet.iter (fun r ->
    if (SMap.mem r tsort.not_yet_visited) then (
      height := max !height (strongconnected r);
      let link_m = SMap.find_unsafe m tsort.links in
      let link_r = SMap.find_unsafe r tsort.links in
      tsort.links <- tsort.links |> SMap.add m (min link_m link_r)
    )
    else if (List.mem r tsort.stack) then (
      let link_m = SMap.find_unsafe m tsort.links in
      let index_r = SMap.find_unsafe r tsort.visit_order in
      tsort.links <- tsort.links |> SMap.add m (min link_m index_r)
    )
    else
      height := max !height (try SMap.find_unsafe r tsort.heights with _ -> 0)
  );

  let link_m = SMap.find_unsafe m tsort.links in
  let index_m = SMap.find_unsafe m tsort.visit_order in
  if (link_m = index_m) then (
    component m (!height+1);
    (!height+1)
  )
  else !height

and component m h =
  let m_ = List.hd tsort.stack in
  tsort.stack <- List.tl tsort.stack;
  tsort.heights <- tsort.heights |> SMap.add m_ h;
  if (m <> m_) then  (
    prerr_endline (spf "CYCLE %s -> %s" m m_);
    tsort.cycle <- true;
    component m h
  )

let height_map modules =
  SMap.fold (fun m height -> fun map ->
    let (file, _) = SMap.find_unsafe m modules in
    let files = try IMap.find_unsafe height map with _ -> [] in
    map |> IMap.add height (file::files)
  ) tsort.heights IMap.empty

let sort_by_height modules =
  IMap.fold (fun h files -> fun partition ->
    Printf.printf "HEIGHT %d: %d\n" h (List.length files);
    files :: partition
  ) (height_map modules) []

let topsort modules =
  tsort.not_yet_visited <- modules;
  tsort.cycle <- false;

  tsort.stack <- [];
  tsort.count <- 0;
  tsort.visit_order <- SMap.empty;
  tsort.links <- SMap.empty;
  tsort.heights <- SMap.empty;

  tarjan ();
  (tsort.cycle, sort_by_height modules)
