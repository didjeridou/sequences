(*
 * ./SuffixArray.ml
 * 
 * Sequences V1
 * Guillaume Laliberté
 * CSCI-51
 * https://github.com/didjeridou/sequences
*)

open Core.Std

exception TODO

module type SUFFIXARRAY =
sig
	type 

	val is_empty : array -> bool
	val create : string -> array
	val search : string -> array -> (bool * int option * int option)
	val invert : array -> array
	(* BWTransform : *)
end