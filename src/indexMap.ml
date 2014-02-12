type read
type write
type no

type (_, _, _, _) t =
  | Read : ('i -> 'e) -> ('i, 'e, no, (read * no)) t
  | Write : ('i -> 'e -> 'u) -> ('i, 'e, 'u, (no * write)) t
  | ReadWrite : ('i -> 'e) * ('i -> 'e -> 'u) -> ('i, 'e, 'u, (read * write)) t

type ('i, 'e) ro = ('i, 'e, no, (read * no)) t
type ('i, 'e, 'u) wo = ('i, 'e, 'u, (no * write)) t
type ('i, 'e, 'u) rw = ('i, 'e, 'u, (read * write)) t

let make_ro get = Read get
let make_wo set = Write set
let make_rw get set = ReadWrite (get, set)

let read : type i e u w. (i, e, u, (read * w)) t -> i -> e =
  fun ct ->
    match ct with
    | Read g -> g
    | ReadWrite (g, _) -> g

let write : type i e u r. (i, e, u, (r * write)) t -> i -> e -> u =
  fun ct ->
    match ct with
    | Write s -> s
    | ReadWrite (_, s) -> s

let get = read
let set = write

let map_ro :
  type i e u w z. (i, e, u, (read * w)) t ->
    (e -> z) ->
    (i, z, no, (read * no)) t
    = fun ct f ->
      match ct with
      | Read g ->
        let get i = f (g i) in
        Read get
      | ReadWrite (g, _) ->
        let get i = f (g i) in
        Read get

let map_wo : type i e u r z. (i, e, u, (r * write)) t ->
  (z -> e) ->
  (i, z, u, (no * write)) t
  = fun ct f ->
    match ct with
    | Write s ->
      let set i x = s i (f x) in
      Write set
    | ReadWrite (_, s) ->
      let set i x = s i (f x) in
      Write set

let map_rw :
  type i e u z. (i, e, u, (read * write)) t ->
    (e -> z) ->
    (z -> e) ->
    (i, z, u, (read * write)) t
    = fun ct fr fw ->
      match ct with
      | ReadWrite (g, s) ->
        let get i = fr (g i) in
        let set i x = s i (fw x) in
        ReadWrite (get, set)

let mapi :
  type i e u p j. (i, e, u, p) t ->
    (j -> i) ->
    (j, e, u, p) t
    = fun ct f ->
      match ct with
      | Read g ->
        let get j = g (f j) in
        Read get
      | Write s ->
        let set j x = s (f j) x in
        Write set
      | ReadWrite (g, s) ->
        let get j = g (f j) in
        let set j x = s (f j) x in
        ReadWrite (get, set)

let to_ro :
  type i e u w. (i, e, u, (read * w)) t ->
    (i, e, no, (read * no)) t
    = function
      | Read _ as r -> r
      | ReadWrite (g, _) -> Read g

let to_wo :
  type i e u r. (i, e, u, (r * write)) t ->
    (i, e, u, (no * write)) t
    = function
      | Write _ as w -> w
      | ReadWrite (_, s) -> Write s

let of_array a = make_rw (Array.get a) (Array.set a)
let of_arrays m = make_rw (fun (i, j) -> m.(i).(j)) (fun (i, j) x -> m.(i).(j) <- x)

let of_array1 ba = make_rw (Bigarray.Array1.get ba) (Bigarray.Array1.set ba)
let of_array2 ba = make_rw (fun (i, j) -> Bigarray.Array2.get ba i j) (fun (i, j) x -> Bigarray.Array2.set ba i j x)
let of_array3 ba = make_rw (fun (i, j, k) -> Bigarray.Array3.get ba i j k) (fun (i, j, k) x -> Bigarray.Array3.set ba i j k x)
let of_genarray ba = make_rw (Bigarray.Genarray.get ba) (Bigarray.Genarray.set ba)

let to_row_major ct ~columns = mapi ct (fun (i, j) -> i * columns + j)
let to_column_major ct ~rows = mapi ct (fun (i, j) -> i + rows * j)

module Tuple2 = struct
  let get ct i j = get ct (i, j)
  let set ct i j e = set ct (i, j) e

  let fix_first ct i = mapi ct (fun j -> (i, j))
  let fix_second ct j = mapi ct (fun i -> (i, j))

  let transpose ct = mapi ct (fun (i, j) -> (j, i))
end

module Tuple3 = struct
  let get ct i j k = get ct (i, j, k)
  let set ct i j k e = set ct (i, j, k) e

  let fix_first ct i = mapi ct (fun (j, k) -> (i, j, k))
  let fix_second ct j = mapi ct (fun (i, k) -> (i, j, k))
  let fix_third ct k = mapi ct (fun (i, j) -> (i, j, k))

  let fix_first_second ct i j = mapi ct (fun k -> (i, j, k))
  let fix_first_third ct i k = mapi ct (fun j -> (i, j, k))
  let fix_second_third ct j k = mapi ct (fun i -> (i, j, k))
end
