(* Examples of some data structures from Batteries *)
let of_vect v =
  IndexMap.make_ro (fun i -> BatVect.get v i)
let of_map m =
  IndexMap.make_ro (fun key -> BatMap.find key m)

