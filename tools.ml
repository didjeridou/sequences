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

module type TOOLS =
sig
  val get_version : unit -> string
  val get_data_path : unit -> string
  val get_intro : unit -> string
  val get_summary : unit -> string
  val get_console : unit -> string
  val current_seq : string -> unit
  val heading : unit -> unit
  val console : unit -> unit
  val empty_data : unit -> unit
  val not_found : unit -> unit
  val found_at : int -> unit
  val list_data : unit -> unit
  val ask_filename : unit -> unit
  val avail_commands : unit -> unit
  val exit_program : unit -> unit
end

(* Write a module called Math that implements the MATH signature above 
 * We have started the module for you below*)

module Tools: TOOLS =
struct
  let get_version () = "0.03.9";;
  let get_data_path () = "./_data/";;

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

  let get_console () = "[Sequences] > ";;

  let current_seq (s: string) : unit =
    print_string ("\n\n[INFO] Sequence selected: " ^ s) 
  ;;
  
  let console () = 
    Out_channel.output_string stdout (get_console ());
    Out_channel.flush stdout;
  ;;

  let list_data () =
    let avail_files = 
      (if (Sys.is_directory_exn (get_data_path ()))
        then 
          let files_array = Sys.readdir (get_data_path ()) in
            Array.fold_right 
              ~f:(fun s a -> (s^"\n-   ")^a) 
              files_array 
              ~init:"\nData files available in ./_data\n\n"
      else "Directory ./_data not found.")
    in print_string avail_files
    ;;

  let empty_data () = 
    print_string "Some data seems to be empty\n"
  ;;

  let not_found () = 
    print_string "\n Not Found. For strings, search without quotes \n"
  ;;

  let found_at index = 
    print_string ("\nFound your pattern at position " 
      ^ (string_of_int index)^"\n\n")
  ;;

  let ask_filename () = 
    print_string "Enter the name of a CFNA file:"
  ;;

  let exit_program () = 
    print_string "Exiting Sequences\n"
  ;;

  let avail_commands () = 
    print_string
"
=== (sub) Command List ===

  commands      List the available commands
  data          List the data files in ./_data/
  exit          Exit Sequences
  load FILE     Load a CFNA file (files in ./_data/)
                No need to write './_data/' or '.cfna'
                ex: load ebola_r -> loads ./_data/ebola_r.cfna
  search STRING Search a STRING pattern in the loaded data
  lcp           Find the LCP in the loaded data
  lcp2 FILE1 FILE2     Find the LCP of two sequences


";;

end
