open Core.Std

exception TODO

module type DNASequence =
sig
	type base
	type sequence
	
	val is_empty : sequence -> bool
	val create : string -> sequence
	val search : sequence -> sequence -> (bool * int option * int option)
	val invert : sequence -> sequence
	val string_of_sequence : sequence -> string
end