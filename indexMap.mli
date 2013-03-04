(** {1 IndexMap operations } *)

(** Index maps are lazy wrappers around indexable data sources. *)

type ('index, 'element, 'mutability) t
(** An mapping of indexes to elements *)

type ('index, 'element, 'mutability) mutability

val immut : ('index, 'element, [`i]) mutability
val mut : ('index -> 'element -> unit) -> ('index, 'element, [`m]) mutability

val make :
  ?mem:('c -> 'i -> bool) ->
  set:('i, 'e, 'm) mutability ->
  get:('c -> 'i -> 'e) ->
  'c -> ('i, 'e, 'm) t
(** [make ?mem ?mut ~get c] will create {!t} value referencing [c].  [get] is
    used to access elements by their index.
    
    @parameter mem defaults to a function which calls [get], ignoring the
    result.  If a value is returned then the default [mem] returns [true].
    If either [Not_found] or [Invalid_argument] are raised by the call to
    [get] then the default [mem] returns [false].  Any other exception raised
    by [get] will not be caught. *)

val get : ('i, 'e, 'm) t -> 'i -> 'e
(** [get imap i] returns the element indexed by [i] from [imap]. *)

val mem : ('i, 'e, 'm) t -> 'i -> bool
(** [mem imap i] returns [true] if the index [i] has a value associated with
    it. *)

val map : ('i, 'e, [`i]) t -> ('e -> 'e_new) -> ('i, 'e_new, [`i]) t
(** [map imap f] returns a new {!t} based on [imap] with each element
    transformed by [f]. *)

val xmap :
  ('i, 'e, 'm) t -> ('e -> 'e_new) -> ('e_new -> 'e) -> ('i, 'e_new, 'm) t
(** [xmap imap f_to f_from] returns a new {!t} based on [imap] with each element
    transformed by [f_to].  [f_from] is used to propagate {!set} calls back
    through the index chain. *)

val map_index : ('i, 'e, 'm) t -> ('i_new -> 'i) -> ('i_new, 'e, 'm) t
(** [map_index imap f] returns a new {!t} based on [imap].  The index is
    transformed by [f]. *)

external to_immutable : ('i, 'e, 'm) t -> ('i, 'e, [`i]) t = "%identity"
(** [to_immutable imap] will return a new version of [imap] which can not be
    modified. *)

val of_array : 'e array -> (int, 'e, [`m]) t
val of_arrays : 'e array array -> (int * int, 'e, [`m]) t
val of_array1 : ('e, _, _) Bigarray.Array1.t -> (int, 'e, [`m]) t
val of_array2 : ('e, _, _) Bigarray.Array2.t -> (int * int, 'e, [`m]) t
val of_array3 : ('e, _, _) Bigarray.Array3.t -> (int * int * int, 'e, [`m]) t
val of_genarray : ('e, _, _) Bigarray.Genarray.t -> (int array, 'e, [`m]) t
val of_function :
  ?mem:('i -> bool) ->
  set:('i, 'e, 'm) mutability ->
  get:('i -> 'e) ->
  ('i, 'e, 'm) t
(** [of_* container] converts [container] to a {!t}.  [container] is
    unmodified.  If [container] is mutable then any changes to [container]
    will be reflected in the resulting {!t}. *)

val to_row_major : (int, 'e, 'm) t -> columns:int -> (int * int, 'e, 'm) t
val to_column_major : (int, 'e, 'm) t -> rows:int -> (int * int, 'e, 'm) t
(** [to_(row|column)_major imap] converts [imap] to use matrix-like indexing.
    Both functions return values with [(i, j)] indexes. *)

module Tuple2 : sig
  val fix_first : ('i_a * 'i_b, 'e, 'm) t -> 'i_a -> ('i_b, 'e, 'm) t
  val fix_second : ('i_a * 'i_b, 'e, 'm) t -> 'i_b -> ('i_a, 'e, 'm) t
  (** [fix_* imap i] returns a new {!t} with the specified index element fixed
      to the value [i]. *)

  val transpose : ('i_a * 'i_b, 'e, 'm) t -> ('i_b * 'i_a, 'e, 'm) t
  (** [transpose imap] returns a new {!t} with the index elements flipped. *)
end
module Tuple3 : sig
  val fix_first :
    ('i_a * 'i_b * 'i_c, 'e, 'm) t -> 'i_a -> ('i_b * 'i_c, 'e, 'm) t
  val fix_second :
    ('i_a * 'i_b * 'i_c, 'e, 'm) t -> 'i_b -> ('i_a * 'i_c, 'e, 'm) t
  val fix_third :
    ('i_a * 'i_b * 'i_c, 'e, 'm) t -> 'i_c -> ('i_a * 'i_b, 'e, 'm) t
  (** [fix_* imap i] returns a new {!t} with the specified index element fixed
      to the value [i]. *)

  val fix_first_second :
    ('i_a * 'i_b * 'i_c, 'e, 'm) t -> 'i_a -> 'i_b -> ('i_c, 'e, 'm) t
  val fix_first_third :
    ('i_a * 'i_b * 'i_c, 'e, 'm) t -> 'i_a -> 'i_c -> ('i_b, 'e, 'm) t
  val fix_second_third :
    ('i_a * 'i_b * 'i_c, 'e, 'm) t -> 'i_b -> 'i_c -> ('i_a, 'e, 'm) t
  (** [fix_* imap i] returns a new {!t} with the specified index elements fixed
      to the value [i]. *)
end
