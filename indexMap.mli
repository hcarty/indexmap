(** {1 IndexMap operations } *)

(** Index maps are lazy wrappers around indexable data sources. *)

type ('index, 'element) t
(** An mapping of indexes to elements *)

val make : ?mem:('c -> 'i -> bool) -> get:('c -> 'i -> 'e) -> 'c -> ('i, 'e) t
(** [make ?mem ~get c] will create {!t} value referencing [c].  [get] is used to
    access elements by their index.
    
    @parameter mem defaults to a function which calls [get], ignoring the
    result.  If a value is returned then the default [mem] returns [true].
    If either [Not_found] or [Invalid_argument] are raised by the call to
    [get] then the default [mem] returns [false].  Any other exception raised
    by [get] will not be caught. *)

val get : ('i, 'e) t -> 'i -> 'e
(** [get imap i] returns the element indexed by [i] from [imap]. *)

val mem : ('i, 'e) t -> 'i -> bool
(** [mem imap i] returns [true] if the index [i] has a value associated with
    it. *)

val map : ('i, 'e) t -> ('e -> 'e_new) -> ('i, 'e_new) t
(** [map imap f] returns a new {!t} based on [imap] with each element
    transformed by [f]. *)

val map_index : ('i, 'e) t -> ('i_new -> 'i) -> ('i_new, 'e) t
(** [map_index imap f] returns a new {!t} based on [imap].  The index is
    transformed by [f]. *)

val of_array : 'e array -> (int, 'e) t
val of_arrays : 'e array array -> (int * int, 'e) t
val of_array1 : ('e, _, _) Bigarray.Array1.t -> (int, 'e) t
val of_array2 : ('e, _, _) Bigarray.Array2.t -> (int * int, 'e) t
val of_array3 : ('e, _, _) Bigarray.Array3.t -> (int * int * int, 'e) t
val of_genarray : ('e, _, _) Bigarray.Genarray.t -> (int array, 'e) t
val of_function : ?mem:('i -> bool) -> ('i -> 'e) -> ('i, 'e) t
(** [of_* container] converts [container] to a {!t}.  [container] is
    unmodified.  If [container] is mutable then any changes to [container]
    will be reflected in the resulting {!t}. *)

val to_row_major : (int, 'e) t -> columns:int -> (int * int, 'e) t
val to_column_major : (int, 'e) t -> rows:int -> (int * int, 'e) t
(** [to_(row|column)_major imap] converts [imap] to use matrix-like indexing.
    Both functions return values with [(i, j)] indexes. *)

module Tuple2 : sig
  val fix_first : ('i_a * 'i_b, 'e) t -> 'i_a -> ('i_b, 'e) t
  val fix_second : ('i_a * 'i_b, 'e) t -> 'i_b -> ('i_a, 'e) t
  (** [fix_* imap i] returns a new {!t} with the specified index element fixed
      to the value [i]. *)

  val transpose : ('i_a * 'i_b, 'e) t -> ('i_b * 'i_a, 'e) t
  (** [transpose imap] returns a new {!t} with the index elements flipped. *)
end
module Tuple3 : sig
  val fix_first : ('i_a * 'i_b * 'i_c, 'e) t -> 'i_a -> ('i_b * 'i_c, 'e) t
  val fix_second : ('i_a * 'i_b * 'i_c, 'e) t -> 'i_b -> ('i_a * 'i_c, 'e) t
  val fix_third : ('i_a * 'i_b * 'i_c, 'e) t -> 'i_c -> ('i_a * 'i_b, 'e) t
  (** [fix_* imap i] returns a new {!t} with the specified index element fixed
      to the value [i]. *)

  val fix_first_second : ('i_a * 'i_b * 'i_c, 'e) t -> 'i_a -> 'i_b -> ('i_c, 'e) t
  val fix_first_third : ('i_a * 'i_b * 'i_c, 'e) t -> 'i_a -> 'i_c -> ('i_b, 'e) t
  val fix_second_third : ('i_a * 'i_b * 'i_c, 'e) t -> 'i_b -> 'i_c -> ('i_a, 'e) t
  (** [fix_* imap i] returns a new {!t} with the specified index elements fixed
      to the value [i]. *)
end
