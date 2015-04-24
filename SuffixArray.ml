(*
 * ./SuffixArray.ml
 * 
 * Sequences V1
 * Guillaume Laliberté
 * CSCI-51
 * https://github.com/didjeridou/sequences
*)

open Core.Std

type order = Equal | Less | Greater

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
    
    type elt = (string * int)
  	type suffixa = elt list

	let is_empty (sa: suffixa) : bool =
		match sa with
		| [] -> true
		| _ -> false

    let suffix_compare (s1: string) (s2: string) : order =
        let int_c = compare s1 s2 in
            if int_c < 0 then Less
            else if int_c = 0 then Equal
            else Greater

	let rec to_string (sa: suffixa) : string =
		match sa with
		| [] -> "\n\n"
		| (str, i)::tl -> 
			"\n" ^ string_of_int i ^ ": " ^ str ^ to_string tl
	;;

(* 	let radixSort (lst: string list): string list =
		match lst with
		| [] -> []
		| _ -> 
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
				| Some (s, index) ->
					let pivot = 
						(if k < String.length s then
							suffix_compare patt (String.sub s 0 (k))
						else suffix_compare patt s)
					in
                    match pivot with
                    | Less -> bs (subSA sa 0 (mid-1))
                    | Equal -> Some (s, index)
                    | Greater -> bs (subSA sa (mid) (length - 1))

		in bs sa

(*         
    TESTS HERE

            let is_empty (sa: suffixa) : bool =

            let suffix_compare (s1: string) (s2: string) : order =

            let rec to_string (sa: suffixa) : string =

            let rec toSuffixList (str: string) (index: int) : suffixa =

            let rec subSA (sa: suffixa) (from: int) (toend: int) = 

            let create (str: string) : suffixa =
                List.sort compare (toSuffixList str 0)

            let search (patt: string) (sa: suffixa) : (string * int) option =
 *)

end

