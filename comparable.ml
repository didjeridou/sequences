open Core.Std

module type COMPARABLE =
sig
  type t

  val is_empty : t -> bool
  val compare : t -> t -> Ordering.t
  val compare_int : t -> t -> int
  val get_suffix : t -> t
  val length : t -> int
  val first_n : t -> int -> t
  val string_of_suffix : t -> string
(*   val string_of_suffix : t -> string
  val string_of_value : t -> string *)
end



(* An example implementation of our COMPARABLE signature. Use this
 * struct for testing. *)
module StringComparable : COMPARABLE =
struct
  open Order

  type t = string

  let is_empty s = String.is_empty s

  let compare s1 s2 = 
    let o = String.compare s1 s2 in
      if o < 0 then Less
      else if o = 0 then Equal
      else Greater

  let compare_int s1 s2 = String.compare

  let get_suffix e = String.drop_prefix e 1

  let length e = String.length e

  let first_n e n = String.sub e 0 n

  let string_of_suffix s = s
(*   let string_of_t (s, i) = string_of_int i^": "^s
  let string_of_suffix (s, _) = s
  let string_of_index (_, i) = string_of_int i *)

end