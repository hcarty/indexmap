(** {1 IndexMap operations } *)

(** Index maps are generic wrappers around indexable data sources. *)

type read
type write
type no
(** Permissions *)

type ('i, 'e, 'u, 'p) t
(** A mapping of indexes to elements *)

type ('i, 'e) ro = ('i, 'e, no, (read * no)) t
type ('i, 'e, 'u) wo = ('i, 'e, 'u, (no * write)) t
type ('i, 'e, 'u) rw = ('i, 'e, 'u, (read * write)) t
(** Shorter names and type aliases *)

val make_ro : ('i -> 'e) -> ('i, 'e) ro
val make_wo : ('i -> 'e -> 'u) -> ('i, 'e, 'u) wo
val make_rw : ('i -> 'e) -> ('i -> 'e -> 'u) -> ('i, 'e, 'u) rw
(** [make_*] will create a {!t} value using the given get and/or set
    function(s). *)

val read : ('i, 'e, 'u, read * 'w) t -> 'i -> 'e
(** [read imap i] returns the element indexed by [i] from [imap]. *)

val write : ('i, 'e, 'u, 'r * write) t -> 'i -> 'e -> 'u
(** [write imap i x] sets index [i] in [imap] to [x]. *)

val get : ('i, 'e, 'u, read * 'w) t -> 'i -> 'e
(** Alias for [read] *)

val set : ('i, 'e, 'u, 'r * write) t -> 'i -> 'e -> 'u
(** Alias for [write] *)

val map_ro : ('i, 'e, 'u, read * 'w) t -> ('e -> 'z) -> ('i, 'z) ro
val map_wo : ('i, 'e, 'u, 'r * write) t -> ('z -> 'e) -> ('i, 'z, 'u) wo
val map_rw :
  ('i, 'e, 'u, read * write) t ->
  ('e -> 'z) -> ('z -> 'e) ->
  ('i, 'z, 'u) rw
(** [map_* imap] returns a new {!t} based on [imap] with each element
    transformed by the given function(s). *)

val mapi : ('i, 'e, 'u, 'p) t -> ('j -> 'i) -> ('j, 'e, 'u, 'p) t
(** [mapi imap f] returns a new {!t} based on [imap].  The index is
    transformed by [f]. *)

val to_ro : ('i, 'e, 'u, read * 'w) t -> ('i, 'e) ro
(** [to_ro imap] returns a read-only version of [imap] *)

val to_wo : ('i, 'e, 'u, 'r * write) t -> ('i, 'e, 'u) wo
(** [to_wo imap] returns a write-only version of [imap] *)

val of_array : 'e array -> (int, 'e, unit) rw
val of_arrays : 'e array array -> (int * int, 'e, unit) rw
(** Create a {!t} from an array *)

val of_array1 : ('e, _, _) Bigarray.Array1.t -> (int, 'e, unit) rw
val of_array2 : ('e, _, _) Bigarray.Array2.t -> (int * int, 'e, unit) rw
val of_array3 : ('e, _, _) Bigarray.Array3.t -> (int * int * int, 'e, unit) rw
val of_genarray : ('e, _, _) Bigarray.Genarray.t -> (int array, 'e, unit) rw
(** Create a {!t} from a bigarray *)

val to_row_major :
  (int, 'e, 'u, 'p) t -> columns:int -> (int * int, 'e, 'u, 'p) t
val to_column_major :
  (int, 'e, 'u, 'p) t -> rows:int -> (int * int, 'e, 'u, 'p) t
(** [to_(row|column)_major imap] converts [imap] to use matrix-like indexing.
    Both functions return values with [(i, j)] indexes. *)

module Tuple2 : sig
  val get : ('i * 'j, 'e, 'u, read * 'w) t -> 'i -> 'j -> 'e
  val set : ('i * 'j, 'e, 'u, 'r * write) t -> 'i -> 'j -> 'e -> 'u

  val fix_first : ('i * 'j, 'e, 'u, 'p) t -> 'i -> ('j, 'e, 'u, 'p) t
  val fix_second : ('i * 'j, 'e, 'u, 'p) t -> 'j -> ('i, 'e, 'u, 'p) t
  (** [fix_* imap i] returns a new {!t} with the specified index element fixed
      to the value [i]. *)

  val transpose : ('i * 'j, 'e, 'u, 'p) t -> ('j * 'i, 'e, 'u, 'p) t
  (** [transpose imap] returns a new {!t} with the index elements flipped. *)
end
module Tuple3 : sig
  val get : ('i * 'j * 'k, 'e, 'u, read * 'w) t -> 'i -> 'j -> 'k -> 'e
  val set : ('i * 'j * 'k, 'e, 'u, 'r * write) t -> 'i -> 'j -> 'k -> 'e -> 'u

  val fix_first :
    ('i * 'j * 'k, 'e, 'u, 'p) t -> 'i -> ('j * 'k, 'e, 'u, 'p) t
  val fix_second :
    ('i * 'j * 'k, 'e, 'u, 'p) t -> 'j -> ('i * 'k, 'e, 'u, 'p) t
  val fix_third :
    ('i * 'j * 'k, 'e, 'u, 'p) t -> 'k -> ('i * 'j, 'e, 'u, 'p) t
  (** [fix_* imap i] returns a new {!t} with the specified index element fixed
      to the value [i]. *)

  val fix_first_second :
    ('i * 'j * 'k, 'e, 'u, 'p) t -> 'i -> 'j -> ('k, 'e, 'u, 'p) t
  val fix_first_third :
    ('i * 'j * 'k, 'e, 'u, 'p) t -> 'i -> 'k -> ('j, 'e, 'u, 'p) t
  val fix_second_third :
    ('i * 'j * 'k, 'e, 'u, 'p) t -> 'j -> 'k -> ('i, 'e, 'u, 'p) t
  (** [fix_* imap i j] returns a new {!t} with the specified index elements fixed
      to the value [i] and [j]. *)
end
