open Bench

let n = 1_000_000

let index_map_int imap () =
  let accu = ref 0 in
  for i = 0 to n - 1 do
    accu := !accu + IndexMap.get imap i
  done;
  !accu

let index_map_float imap () =
  let accu = ref 0.0 in
  for i = 0 to n - 1 do
    accu := !accu +. IndexMap.get imap i
  done;
  !accu

let raw_array_int a () =
  let accu = ref 0 in
  for i = 0 to n - 1 do
    accu := !accu + a.(i)
  done;
  !accu

let raw_array_float a () =
  let accu = ref 0.0 in
  for i = 0 to n - 1 do
    accu := !accu +. a.(i)
  done;
  !accu

let int_arrays =
  let a = Array.init n (fun x -> x) in
  let index = IndexMap.of_array a in
  [
    "indexed int array", index_map_int index;
    "raw int array", raw_array_int a;
  ]

let float_arrays =
  let a = Array.init n (fun x -> float_of_int x) in
  let index = IndexMap.of_array a in
  [
    "indexed float array", index_map_float index;
    "raw float array", raw_array_float a;
  ]

let () =
  bench int_arrays;
  bench float_arrays

