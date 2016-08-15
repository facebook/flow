let files = ref SMap.empty

let get x = SMap.find_unsafe x !files
let set x y = files := SMap.add x y !files
