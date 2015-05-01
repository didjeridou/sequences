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
    type suffix
    type sarray

    val empty : sarray

    val from_string : string -> sarray
    val string_search : string -> sarray -> (suffix * int) option
    val get_suffix_by_rank : int -> sarray -> suffix option

    val string_of_suffix : suffix -> string
    val string_of_index : int -> string
    val string_of_sarray : sarray -> string

    val lcp_array_of_sarray : sarray -> (int * int * int * int) list

    val lcp_single : (int * int * int * int) list 
        -> (int * int * int * int) option

end

module type SUFFIXARRAY_ARG =
sig
    type suffix

    val compare : suffix -> suffix -> Ordering.t

    val string_of_suffix : suffix -> string
    val string_of_index : int -> string
end


(* List implementation of the SUFFIXARRAY signature. *)
(* Currently implemented for string because that's what
 * Suffix Arrays are for, but keeping the door open for
 * potential new implementations. Is also implemented
 * with list instead of array, but kept the name array
 * for consistency with documentation, *)
module SuffixArray(SA:SUFFIXARRAY_ARG) : (SUFFIXARRAY 
    with type suffix = string) =
struct
    type suffix = string
    type sarray = (suffix * int) list;;

    (* INVARIANT: sorted by index, no duplicates *)

    let empty = [] ;;

    (* Helper for from_string *)
    let rec to_suffix_list (str: string) (index: int) : sarray =
        if String.is_empty str then []
        else
            let suffix = String.drop_prefix str 1 in
                (str, index) :: (to_suffix_list suffix (index + 1))

    (* Creates SA from a string *)
    let from_string (str: string) : sarray =
        List.sort ~cmp:(compare) (to_suffix_list str 0)

    (* Helper for binary search. Returns half-array for range *)
    let rec sub_sa (sa: sarray) (from: int) (toend: int) = 
        match sa with
        | [] -> failwith "sub_sa"
        | hd::tl -> 
            let tail = 
                if toend = 0 then [] 
                else sub_sa tl (from - 1) (toend - 1)
            in
            if from > 0 then tail 
            else hd::tail

    (* Helper function for search *)
    let fix_compare (s1: string) (s2: string) : Ordering.t =
            let int_c = compare s1 s2 in
                if int_c < 0 then Less
                else if int_c = 0 then Equal
                else Greater

    (* Binary search for Suffx Array *)
    let string_search (str: string) (sa: sarray) : (suffix * int) option =
        let k = String.length str in
        let rec bs sa =
            let length = List.length sa in
            let mid = (length / 2) in
                match List.nth sa mid with
                | None -> None
                | Some (s, index) ->
                    let pivot = 
                        (if k < String.length s then
                            fix_compare str (String.sub s ~pos:0 ~len:k)
                        else fix_compare str s)
                    in
                    if mid = 0 then
                        match pivot with
                        | Less | Greater -> None
                        | Equal -> Some (s, index)
                    else
                        match pivot with
                        | Less -> bs (sub_sa sa 0 (mid-1))
                        | Equal -> Some (s, index)
                        | Greater -> bs (sub_sa sa (mid) (length - 1))
        in bs sa

    let get_suffix_by_rank (r: int) (sa: sarray) : suffix option =
        match List.nth sa r with
        | None -> None
        | Some (s,_) -> Some s

    (* HELPER: Longest common substring *)
    let rec lcs (s1: string) (s2: string) (c: int) : int =
        match fix_compare (String.prefix s1 1) (String.prefix s2 1) with
        | Less | Greater -> c
        | Equal ->
            if s1 = "" || s2 = "" then c
            else
                lcs (String.drop_prefix s1 1) (String.drop_prefix s2 1) (c+1)

    (* Creates and LCP array of an SA *)
    let lcp_array_of_sarray (sarr: sarray) : (int * int * int * int) list =
        let rec lcp (sa: sarray) (index: int) =
            match List.nth sa 0, List.nth sa 1 with
            | None, _ | _, None -> []
            | Some (s1,i1), Some (s2,i2) -> 
                ((lcs s1 s2 0), i1, i2, index)::(lcp (List.drop sa 1) (index+1))
        in lcp sarr 0

    (* Finds the LCP for a given LCP array *)
    let rec lcp_single (lst: (int * int * int * int) list) 
        : (int * int * int * int) option =
        match lst with
        | [] -> None
        | hd::tl -> max (Some hd) (lcp_single tl)

    let string_of_suffix s = s
    let string_of_index = string_of_int
    let string_of_sarray (sa: sarray) : string = 
        let rec to_string sa =
            match sa with
            | [] -> "\n\n"
            | (str, i)::tl -> 
                "\n" ^ string_of_int i ^ ": " ^ str ^ to_string tl
        in to_string sa
end


module Make (SA: SUFFIXARRAY_ARG) : (SUFFIXARRAY 
    with type suffix = string) =
    
    SuffixArray(SA)




