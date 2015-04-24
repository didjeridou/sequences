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
	val is_empty : (string*int) list -> bool
	val create : string -> (string * int) list
 	val search : string -> (string * int) list -> (string * int) option
	(* val invert : array -> array *)
end

module SuffixArray: SUFFIXARRAY =
struct
	let is_empty (lst: (string * int) list) : bool =
		match lst with
		| [] -> true
		| _ -> false

(* 	let radixSort (lst: string list): string list =
		match lst with
		| [] -> []
		| _ -> List.sort compare lst
 *)
	let rec toSuffixList (str: string) (index: int) : (string*int) list =
		if String.is_empty str then [("$", index)]
		else
			let suffix = String.drop_prefix str 1 in
				(str^"$", index) :: (toSuffixList suffix (index + 1))

	let rec sublist lst (from: int) (toend: int) = 
	  	match lst with
	  	| [] -> failwith "sublist"
	  	| hd::tl -> 
	    	let tail = 
	    		if toend = 0 then [] 
	    		else sublist tl (from - 1) (toend - 1)
	    	in
	     	if from > 0 then tail 
	     	else hd::tail

(*
sublist [1;2;3;4;5;6;7;8;9] 3 5;;
- : int list = [4; 5; 6]
*)

	let rec create (str: string) : (string * int) list =
		List.sort compare (toSuffixList str 0)

	let rec search (patt: string) (sa: (string * int) list) : (string * int) option =
		let rec bs sa =
			let length = List.length sa in
			let mid = (length / 2) in
				match List.nth sa mid with
				| None -> None
				| Some (str, index) -> 
					ignore (print_string patt);
					ignore (print_string str);
					ignore (print_int (compare patt str));
					let pivot = compare patt str in
						if pivot < 0
							then bs (sublist sa 0 (mid-1))
						else if pivot = 0
							then Some (str, index)
						else bs (sublist sa (mid) length)
		in bs sa

end

