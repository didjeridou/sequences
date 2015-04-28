(*
 * ./sequences.ml
 * 
 * Sequences V1
 * Guillaume Laliberté
 * CSCI-51
 * https://github.com/didjeridou/sequences
*)

(** example.ml: The first special comment of the file is the comment
    associated with the whole module. *)

open Core.Std
open DNASequence





let verbose = ref false

let speclist = 
	[
		("-v", Arg.Set verbose, "Enables verbose mode");
	]
in 
let usage_msg = "Options available:"in 
	Arg.parse speclist print_endline usage_msg;
	print_endline ("Verbose mode: " ^ string_of_bool !verbose);


module DNA = DNASequence.Make(
  struct
    type t = string
    let compare s1 s2 =
    	let int_c = compare s1 s2 in
	        if int_c < 0 then Less
	        else if int_c = 0 then Equal
	        else Greater

    let string_of_t t = t
  end)

let string_of_result (str: string) (dna: DNA.seq) : string = 
	match DNA.pattern_search str dna with
	| None -> "\n Not Found \n"
	| Some (s, i) -> 
		("\nFound " ^ string_of_int i ^ ": " ^ DNA.string_of_subseq s)
;;

let test_sa = DNA.from_string "atcag";;

print_string (DNA.to_string test_sa);;
print_string (string_of_result "g" test_sa);;