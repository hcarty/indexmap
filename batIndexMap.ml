(* Examples of some data structures from Batteries *)
let of_vect v =
  let length = BatVect.length v in
  let mem i = i >= 0 && i < length in
  IndexMap.of_function ~mem ~set:IndexMap.immut ~get:(fun i -> BatVect.get v i)
let of_map m =
  IndexMap.of_function 
    ~mem:(fun key -> BatMap.mem key m)
    ~set:IndexMap.immut
    ~get:(fun key -> BatMap.find key m)

