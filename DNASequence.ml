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
	val lcp_array_from_seq : seq -> (int * int * int * int) list

	val lcp_single : (int * int * int * int) list 
		-> (int * int * int * int) option

	val string_of_seq : seq -> string
	val string_of_subseq : subseq option -> string
	val string_of_lcp_single : (int * int * int * int) option -> seq 
		-> string

	val string_of_lcp_double : (int * int * int * int) option -> seq 
		-> string

	val to_string : seq -> string
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

	let lcp_array_from_seq (s: seq) : (int * int * int * int) list =
		SA.lcp_array_of_sarray s

	let lcp_single (l: (int * int * int * int) list) 
		: (int * int * int * int) option = 
		SA.lcp_single l

	let string_of_seq (dna: seq) : string =
		SA.string_of_sarray dna

	let string_of_subseq (sub: subseq option) : string =
		match sub with
		| None -> "Empty subsequence"
		| Some s -> SA.string_of_suffix s

	let string_of_lcp_single (lcp: (int * int * int * int) option) 
		(data: seq) : string =
		match lcp with
		| None -> "\nNo common prefix found \n"
		| Some (l, posA, posB, i) -> 
		        ("\n\nFound an LCP of length " ^ string_of_int l 
		          ^ " at position " ^ string_of_int (posA+1) ^ " and " 
		          ^ string_of_int (posB+1) ^ "\n***LCP START***\n"
		      	  ^ String.prefix (string_of_subseq (
		      	  		SA.get_suffix_by_rank i data)) l
		      	  ^ "\n***LCP END***\n\n")

    let string_of_lcp_double (lcp: (int * int * int * int) option) 
    	(data: seq) : string =
		match lcp with
		| None -> "\nNo common prefix found \n"
		| Some (l, _, _, i) -> 
		        ("\n\nFound an LCP of length " ^ string_of_int l 
		          ^ "\n***LCP START***\n" 
		          ^ String.prefix (string_of_subseq (
		          		SA.get_suffix_by_rank i data)) l
		      	  ^ "\n***LCP END***\n\n")

	let to_string = string_of_seq

end

module Make (C : COMPARABLE) : (DNASEQ) =
	SuffixArrayDNASeq (C)




