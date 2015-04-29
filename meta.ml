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
  val get_console : unit -> string
  val current_seq : string -> unit
  val heading : unit -> unit
  val console : unit -> unit
  val empty_data : unit -> unit
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

                   SEQUENCES V "^ get_version () ^"

==========================================================

-._    _.--'¨`'--._    _.--'¨`'--._    _.--'¨`'--._    _  
    '-:`.'|`|¨':-.  '-:`.'|`|¨':-.  '-:`.'|`|¨':-.  '.` : 
  '.  '.  | |  | |'.  '.  | |  | |'.  '.  | |  | |'.  '.: 
  : '.  '.| |  | |  '.  '.| |  | |  '.  '.| |  | |  '.  '.
  '   '.  `.:_ | :_.' '.  `.:_ | :_.' '.  `.:_ | :_.' '.  
         `-..,..-'       `-..,..-'       `-..,..-'       `
";;

  let get_summary () =
"\n[INFO] SEQUENCES indexes a DNA sequence as a suffix array 
(from a CFNA file) for efficient analysis."
  
  let heading () = 
    print_string ((get_intro ()) ^ (get_summary ()))
  ;;

  let get_console () = "Sequences > ";;

  let current_seq (s: string) : unit =
    print_string ("[INFO] Sequence selected: " ^ s 
      ^ "\n\n[STATUS] Perfom analysis using the following commands.\n\n");;
  
  let console () = 
    Out_channel.output_string stdout (get_console ());
    Out_channel.flush stdout;
  ;;

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

  let empty_data () = 
    print_string "Data seems to be empty\n"
  ;;

  let ask_filename () = print_string "Enter the name of a CFNA file:"
  ;;

  let avail_commands () = 
    print_string
"
=== commands ===

  data          List the CFNA data in ./_data
  search SEQ    Search the DNA for a certain string SEQ
  searchf SEQ   Search the DNA for a SEQ from a .cfna file
  lcp SEQ       Find Longest Common Prefix in DNA & file SEQ
  exit          Find Longest Common Prefix in DNA & file SEQ


";;

end
