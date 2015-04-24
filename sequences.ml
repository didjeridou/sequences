(*
 * ./sequences.ml
 * 
 * Sequences V1
 * Guillaume Laliberté
 * CSCI-51
 * https://github.com/didjeridou/sequences
*)

open Core.Std
open SuffixArray

let test_sa = SuffixArray.create "aacaagtttacaagc"

let rec string_of_sa (slist: (string*int) list) : string =
	match slist with
	| [] -> "\n\n"
	| (str, i)::tl -> 
		"\n" ^ string_of_int i ^ ": " ^ str ^ string_of_sa tl
;;

print_string (string_of_sa test_sa);;

let string_of_result (slist: (string*int) list) : string = 
	match SuffixArray.search "aagc$" slist with
	| None -> "\n Not Found \n"
	| Some (str, i) -> ("\n" ^ string_of_int i ^ ": " ^ str)
;;

print_string (string_of_result test_sa);;
