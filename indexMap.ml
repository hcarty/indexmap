type ('index, 'element, 'mutability) t = {
  get : 'index -> 'element;
  set : 'index -> 'element -> unit;
  mem : 'index -> bool;
}

type ('index, 'element, 'mutability) mutability =
  | Mutable of ('index -> 'element -> unit)
  | Immutable

let immut = Immutable
let mut set = Mutable set

let mem_default c get i =
  try
    ignore (get c i);
    true
  with
  | Not_found
  | Invalid_argument _ -> false

let make ?mem ~set ~get container =
  let mem =
    match mem with
    | Some f -> fun i -> f container i
    | None -> fun i -> mem_default container get i
  in
  let get i = get container i in
  let set =
    match set with
    | Immutable -> assert false
    | Mutable f -> f
  in
  { get; set; mem }

let get { get; _ } i = get i
let set { set; _ } i x = set i x
let mem { mem; _ } i = mem i

let map index f = {
  index with
    get = (fun i -> f (index.get i));
    set = (fun _i _x -> assert false);
}

let xmap index f_to f_from = {
  index with
    get = (fun i -> f_to (index.get i));
    set = (fun i x -> index.set i (f_from x));
}

let map_index index f = {
  get = (fun i -> index.get (f i));
  set = (fun i x -> index.set (f i) x);
  mem = (fun i -> index.mem (f i));
}

external to_immutable : ('i, 'e, 'm) t -> ('i, 'e, [`i]) t = "%identity"

let of_array a =
  let length = Array.length a in
  let mem i = i >= 0 && i < length in
  { mem; get = (fun i -> Array.get a i); set = (fun i x -> Array.set a i x) }

(* No custom mem functions for bigarrays because of the difference in initial
    index between C and Fortran layouts. *)
let of_array1 a =
  make
    ~set:(Mutable (fun i x -> Bigarray.Array1.set a i x))
    ~get:Bigarray.Array1.get
    a
let of_array2 a =
  make
    ~set:(Mutable (fun (i, j) x -> Bigarray.Array2.set a i j x))
    ~get:(fun a (i, j) -> Bigarray.Array2.get a i j)
    a
let of_array3 a =
  make
    ~set:(Mutable (fun (i, j, k) x -> Bigarray.Array3.set a i j k x))
    ~get:(fun a (i, j, k) -> Bigarray.Array3.get a i j k)
    a
let of_genarray a =
  make
    ~set:(Mutable (fun i x -> Bigarray.Genarray.set a i x))
    ~get:Bigarray.Genarray.get
    a

let of_function ?mem ~set ~get =
  let mem =
    match mem with
    | Some f -> f
    | None -> begin
      fun i ->
        try
          ignore (get i);
          true
        with
        | Not_found
        | Invalid_argument _ -> false
    end
  in
  let set =
    match set with
    | Immutable -> assert false
    | Mutable f -> f
  in
  { get; mem; set }

let of_arrays a =
  of_function
    ?mem:None
    ~set:(Mutable (fun (i, j) x -> a.(i).(j) <- x))
    ~get:(fun (i, j) -> a.(i).(j))

let to_row_major index ~columns =
  map_index index (fun (i, j) -> i * columns + j)
let to_column_major index ~rows =
  map_index index (fun (i, j) -> i + rows * j)

module Tuple2 = struct
  let fix_first index i = map_index index (fun j -> (i, j))
  let fix_second index j = map_index index (fun i -> (i, j))

  let transpose index = map_index index (fun (i, j) -> (j, i))
end
module Tuple3 = struct
  let fix_first index i = map_index index (fun (j, k) -> (i, j, k))
  let fix_second index j = map_index index (fun (i, k) -> (i, j, k))
  let fix_third index k = map_index index (fun (i, j) -> (i, j, k))

  let fix_first_second index i j = map_index index (fun k -> (i, j, k))
  let fix_first_third index i k = map_index index (fun j -> (i, j, k))
  let fix_second_third index j k = map_index index (fun i -> (i, j, k))
end

