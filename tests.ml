
#use "topfind";;
#thread;;
#camlp4o;;
#require "core.top";;
#require "core.syntax";;
open Core.Std;;

    let fix_compare (s1: string) (s2: string) : Ordering.t =
            let int_c = compare s1 s2 in
                if int_c < 0 then Less
                else if int_c = 0 then Equal
                else Greater

    assert(fix_compare "asdf" "asdf" = Equal);
    assert(fix_compare "a" "f" = Less);
    assert(fix_compare "r" "b" = Greater);
    assert(fix_compare "ra" "b" = Greater);
    assert(fix_compare "rat" "ras" = Greater);

    let rec lcs (s1: string) (s2: string) (c: int) : int =
        match fix_compare (String.prefix s1 1) (String.prefix s2 1) with
        | Less | Greater -> c
        | Equal ->
            if s1 = "" || s2 = "" then c
            else
                lcs (String.drop_prefix s1 1) (String.drop_prefix s2 1) (c+1)

    assert(lcs "a" "a" 0 = 1);

    type sarray = (string * int) list ;;

    let test_a = [
        ("abcdbcd", 0);
        ("bcd", 4);
        ("bcdbcd", 1);
        ("cd", 5);
        ("cdbcd", 2);
        ("d", 6);
        ("dbcd", 3)
    ];;

    let lcp_array_of_sarray (sarr: sarray) : int list =
        let rec lcp (sa: sarray) =
            match List.nth sa 0, List.nth sa 1 with
            | None, _ | _, None -> []
            | Some (s1,_), Some (s2,_) -> (lcs s1 s2 0)::(lcp (List.drop sa 1))
        in lcp sarr

    let lcp_max (lcplst: int list) = 
        let rec max_lcp (lst: int list) (lcp: int) : int =
            match lst with
            | [] -> 0
            | _::[] -> lcp
            | hd1::hd2::tl ->
                if hd1 = hd2 then max_lcp tl (max lcp hd1)
                else max_lcp tl lcp
        in max_lcp lcplst 0