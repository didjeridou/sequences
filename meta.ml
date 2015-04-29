(*
 * ./out.ml
 * 
 * Sequences V1
 * Guillaume Laliberté
 * CSCI-51
 * https://github.com/didjeridou/sequences
*)

open Core.Std

(* Just the text for the command line *)

module type META =
sig
  val get_version : unit -> string
  val get_intro : unit -> string
  val get_summary : unit -> string
  val input : unit -> unit
  val data_files : unit -> unit
  val ask_filename : unit -> unit
  val avail_commands : unit -> unit
end

(* Write a module called Math that implements the MATH signature above 
 * We have started the module for you below*)

module Meta: META =
struct
  let get_version () = "0.03.7";;
  
  let get_intro () =
"
==========================================================

                   DNA Pattern Finder

==========================================================

-._    _.--'¨`'--._    _.--'¨`'--._    _.--'¨`'--._    _  
    '-:`.'|`|¨':-.  '-:`.'|`|¨':-.  '-:`.'|`|¨':-.  '.` : 
  '.  '.  | |  | |'.  '.  | |  | |'.  '.  | |  | |'.  '.: 
  : '.  '.| |  | |  '.  '.| |  | |  '.  '.| |  | |  '.  '.
  '   '.  `.:_ | :_.' '.  `.:_ | :_.' '.  `.:_ | :_.' '.  
         `-..,..-'       `-..,..-'       `-..,..-'       `
";;

  let get_summary () =
"\nIndexes a DNA sequence as a suffix array (from a CFNA file)
for efficient analysis."

  let input () = print_string "Sequences >";;

  let data_files () =
    let avail_files = 
      (if (Sys.is_directory_exn "./_data")
        then 
          let files_array = Sys.readdir "./_data" in
            Array.fold_right 
              ~f:(fun s a -> (s^"\n-   ")^a) 
              files_array 
              ~init:"\nData files available in ./_data\n"
      else "Directory ./_data not found.")
    in print_string avail_files
    ;;

  let ask_filename () = print_string "Enter the name of a CFNA file:"
  ;;

  let avail_commands () = 
    print_string
"
=== commands ===

  search SEQ    Search the DNA for a certain string SEQ
  searchf SEQ   Search the DNA for a SEQ from a cfna file
  lcp SEQ       Find Longest Common Prefix in DNA & file SEQ
  exit          Find Longest Common Prefix in DNA & file SEQ


";;

end
