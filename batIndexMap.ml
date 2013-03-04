(* Examples of some data structures from Batteries *)
let of_vect v =
  let length = Vect.length v in
  let mem i = i >= 0 && i < length in
  IndexMap.of_function ~mem ~get:(fun i -> Vect.get v i)
let of_map m =
  IndexMap.of_function 
    ~mem:(fun key -> Map.mem key m)
    ~get:(fun key -> Map.find key m)

