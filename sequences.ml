(*
 * ./sequences.ml
 * 
 * Sequences V1
 * Guillaume Laliberté
 * CSCI-51
 * https://github.com/didjeridou/sequences
*)

open Core.Std
open DNASequence
(* open SuffixArray

let test_sa = SuffixArray.create "aacaagtttacaagc";;

print_string (SuffixArray.to_string test_sa);;

let string_of_result (patt: string) (sa: SuffixArray.suffixa) : string = 
	match SuffixArray.search patt sa with
	| None -> "\n Not Found \n"
	| Some (str, i) -> ("\n Found " ^ string_of_int i ^ ": " ^ str)
;;

print_string (string_of_result "aagc" test_sa);;
print_string (string_of_result "ca" test_sa);;
 *)


module DNASeq = DNASequence.Make(
  struct
    type t = string
    let compare s1 s2 =
    	let int_c = compare s1 s2 in
	        if int_c < 0 then Less
	        else if int_c = 0 then Equal
	        else Greater
	        
    let string_of_t t = t
  end)