(*
 * ./sequences.ml
 * 
 * Sequences V1
 * Guillaume Laliberté
 * CSCI-51
 * https://github.com/didjeridou/sequences
*)

(* FASTA Format DNA and Protein Sequence Alignment
 * http://en.wikipedia.org/wiki/FASTA_format *)
(* ftp://ftp.ncbi.nih.gov/genomes/Viruses/ *)

(** example.ml: The first special comment of the file is the comment
    associated with the whole module. *)

open Core.Std
open DNASequence
open Meta


(* Just a small pattern to make quick tests *)
let data_test_pattern = "e";;
let opened_cfna = ref "No opened CFNA file";;

(* declare x to be a mutable variable of type “int ref” 
let x = ref 3;;
(* reset the value of x *)
x := 4;;
(* syntax for getting the value of x *)
!x;;
x := !x + 1;;  (* increase x by 1 *)
print_int !x;;
*)

(* We create our module DNA to use strings as prefixes since
 * DNA data is in fact a series of A,T,C and G *)
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
  | Some (_, i) -> 
    ("\nFound Your Pattern at Position " ^ string_of_int i)
(*    ("\nFound " ^ string_of_int i ^ ": " ^ DNA.string_of_subseq s) *)
;;

(* Open a .dna file and make it a string for use with our data struct *)
let parse_cfna file =
  In_channel.with_file file ~f:(fun ic ->
    ignore(opened_cfna := input_line ic);
    input_line ic
  )

(* Helper function to index a CFNA into our DNA data structure *)
let dna_from_cfna file : DNA.seq option = 
  ignore(print_string "\n\n%% Indexing DNA...\n";);
  Some (DNA.from_string (parse_cfna file))
(*     print_string (
      string_of_result test_pattern test_sequence); *)
;;

let rec open_to_analyze (data: DNA.seq option) =
  ignore(Meta.indexed ());
  ignore(Meta.avail_commands ());
  ignore(Meta.input ());

  match data with
  | None -> Meta.empty_data ()
  | Some d ->
    print_string (
      string_of_result data_test_pattern d)
;;


let use_cfna =
  Command.basic 
    ~summary:"Indexes a base DNA (.cfna) file to perform operations"
    Command.Spec.(
      empty
      +> anon ("filename" %: file)
    )
    (fun filename () -> 
      (Meta.heading ());
      open_to_analyze (dna_from_cfna filename))

let default =
  Command.basic 
    ~summary:"Open Sequences without a file"
    Command.Spec.(
      empty
    )
    (fun () -> 
      (Meta.heading ());
      open_to_analyze None)

let command =
  Command.group 
    ~summary: ((Meta.get_intro ()) ^ (Meta.get_summary ()))
    [ "use", use_cfna ]

let () = Command.run ~version: (Meta.get_version ()) ~build_info:"DNA" command

