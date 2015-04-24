(*
 * ./SuffixArray.ml
 * 
 * Sequences V1
 * Guillaume Laliberté
 * CSCI-51
 * https://github.com/didjeridou/sequences
*)

open Core.Std

module type SUFFIXARRAY =
sig
  	type suffixa

	val is_empty : suffixa -> bool
	val to_string : suffixa -> string

	val create : string -> suffixa
 	val search : string -> suffixa -> (string * int) option

end

module SuffixArray: SUFFIXARRAY =
struct

  	type suffixa = (string * int) list

	let is_empty (sa: suffixa) : bool =
		match sa with
		| [] -> true
		| _ -> false

	let rec to_string (sa: suffixa) : string =
		match sa with
		| [] -> "\n\n"
		| (str, i)::tl -> 
			"\n" ^ string_of_int i ^ ": " ^ str ^ to_string tl
	;;

(* 	let radixSort (lst: string list): string list =
		match lst with
		| [] -> []
		| _ -> List.sort compare lst
 *)
	let rec toSuffixList (str: string) (index: int) : suffixa =
		if String.is_empty str then []
		else
			let suffix = String.drop_prefix str 1 in
				(str, index) :: (toSuffixList suffix (index + 1))

	let rec subSA (sa: suffixa) (from: int) (toend: int) = 
	  	match sa with
	  	| [] -> failwith "subSA"
	  	| hd::tl -> 
	    	let tail = 
	    		if toend = 0 then [] 
	    		else subSA tl (from - 1) (toend - 1)
	    	in
	     	if from > 0 then tail 
	     	else hd::tail
(*
subSA [1;2;3;4;5;6;7;8;9] 3 5;;
- : int list = [4; 5; 6]
*)

	let create (str: string) : suffixa =
		List.sort compare (toSuffixList str 0)

	let search (patt: string) (sa: suffixa) : (string * int) option =
		let k = String.length patt in
		let rec bs sa =
			let length = List.length sa in
			let mid = (length / 2) in
				match List.nth sa mid with
				| None -> None
				| Some (str, index) ->
					(* ignore(print_string "\n String: ");
					ignore(print_int (String.length str));
					ignore(print_string (", "^str));

					ignore(print_string "\n k: ");
					ignore(print_int k); *)
					let pivot = 
						(if k < String.length str then
							compare patt (String.sub str 0 (k))
						else compare patt str)
					in
					if pivot < 0
						then bs (subSA sa 0 (mid-1))
					else if pivot = 0
						then Some (str, index)
					else 
						bs (subSA sa (mid) (length - 1))
		in bs sa



end

