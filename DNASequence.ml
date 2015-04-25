(*
 * ./DNASequence.ml
 * 
 * Sequences V1
 * Guillaume Laliberté
 * CSCI-51
 * https://github.com/didjeridou/seqs
*)

open Core.Std

(* The interface for the DNA Sequence *)
module type DNASEQ =
sig
	(* abstract seq type *)
	type seq
	type subseq
	
	val empty : seq

	val is_empty : seq -> bool

	val from_string : string -> seq
	val pattern_search: string -> seq -> (subseq * int) option
	val string_of_seq : seq -> string
	val to_string : seq -> string
	val string_of_subseq : subseq -> string
end

module type COMPARABLE =
sig
  type t
  val compare : t -> t -> Ordering.t
  val string_of_t : t -> string
end



module SuffixArrayDNASeq(C : COMPARABLE) : (DNASEQ) =
struct
    module SA = SuffixArray.Make(
    struct
      	type suffix = C.t

      	let compare s1 s2 = C.compare s1 s2
      	let string_of_suffix s = C.string_of_t s
      	let string_of_index i = string_of_int i

	end)

    type subseq = SA.suffix
	type seq = SA.sarray

	let empty = SA.empty

	let is_empty (dna: seq) : bool =
		if dna = SA.empty then true else false

	let from_string (str: string) : seq =
		SA.from_string str

	let pattern_search (str: string) (dna: seq) : (subseq * int) option =
		SA.string_search str dna

	let string_of_seq (dna: seq) : string =
		SA.string_of_sarray dna

	let to_string = string_of_seq

	let string_of_subseq (s: subseq) : string =
		SA.string_of_suffix s

end

module Make (C : COMPARABLE) : (DNASEQ) =
	SuffixArrayDNASeq (C)




