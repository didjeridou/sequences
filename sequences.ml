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

(* ABOUT MODULES:
 * DNASequence is the module for, you guessed it, a
 * DNA sequence. It is built over a string suffix array
 * 
 * Tools is a custom module that isolates useful functions
 * like printing the header of the program at launch or 
 * showing the custom console for executing commands. *)
open Core.Std
open DNASequence
open Tools

(* Module DNA to manipulate strings of DNA as suffix since
 * DNA data is in fact a series of A,T,C and G.
 * See the full implementation (as string suffix arrays)
 * in DNASequence.ml *)
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

(* ABOUT .cfna FILES:
 * .fna files are for the FASTA Format, a special format fo
 * ATCG sequences. All the data used in this demo was obtained
 * as .fna files from ftp://ftp.ncbi.nih.gov/genomes/
 *
 * We use a custom format, .cfna or condensed .fna which is
 * basically the fna file without the newline characters.
 *
 * CONVENTION: 
 * All reference names are preceded by '_' for
 * a better readability / quick identification
 *
 * _data: ref to an empty DNA seq. Will hold our final DNA.seq
 * _cfna: ref to a string containing the CFNA meta data.
 * _subcommands: ref to what will be our 'controls' for the program.
 * It is a Command group for the shell. *)

let _data = ref DNA.empty;;
let _cfna = ref "No opened CFNA file";;
let _subcommands =
  ref (Command.group 
        ~summary: ("")
        [])
;;

(* HELPER: parses a .cfna file. 
 * Get the metadata, sets the _cfna metadata and get the string 
 * of ATCG to create the DNA.seq *)
let parse_cfna file =
  In_channel.with_file file ~f:(fun ic ->
    _cfna := input_line ic;
    Tools.current_seq !_cfna;
    let atcg = input_line ic in 
      Some atcg)

(* HELPER: index the ATCG data from the CFNA into our DNA
 * data structure (built over a string suffix array) and
 * set _data to our new data *)
let load_cfna file = 
  let parsed = parse_cfna file in
    match parsed with
    | None -> print_string "No ATCG in file in selected file"
    | Some loaded -> 
      print_string "\n\n[STATUS] Indexing DNA...\n\n";
      _data := (DNA.from_string loaded);
;;

(* Main program where we show the console to the user.
 * Using subcommands, the user will be able to control
 * the program and perform operations on the DNA (_data) *)

let rec main () =
  Tools.console ();

  match In_channel.input_line stdin with
  | None -> main ()
  | Some cmd ->
    Command.run 
      ~argv: ("unrecognized"::(String.split cmd ~on:' '))
      !_subcommands
;;

(* COMMANDS & SUBCOMMANDS
 * Here are all the available commands and subcommands.
 * Commands are arguments that can be passed to 
 * ./sequences.native while subcommands can be used once
 * Sequences is launched. You will see 
 *    [Sequences] >
 * when the program runs. There you can run subcommands*)


(*###########
 * COMMANDS
 *##########*)

(* Load the base file in the memory
 * This loads the file and creates a new DNA.seq, itself built
 * over a suffix array. *)
let load_cfna_cmd =
  Command.basic 
    ~summary:"Indexes a base DNA (.cfna) file to perform operations"
    Command.Spec.(
      empty
      +> anon ("filename" %: file)
    )
    (fun filename () -> 
      Tools.heading ();
      load_cfna filename;
      print_string 
        "[STATUS] Indexing complete
        \n[INFO] Perfom analysis using the following commands.\n";
      Tools.avail_commands ();
      main ())
;;

(* Associating textual commands to actions *)
let command =
  Command.group ~summary: (Tools.get_intro () ^ Tools.get_summary ())
    [ "load", load_cfna_cmd ]
;;

(*##############
 * SUBCOMMANDS
 *#############*)

(* Subcommand 'exit' *)
let exit_program =
  Command.basic ~summary:"Exit Sequences" Command.Spec.(empty)
    (fun () -> Tools.exit_program ())
;;

(* Subcommand 'data', lists the files available in our data
 * folder, ./_data *)
let list_data =
  Command.basic ~summary:"List the CFNA data in ./_data"
    Command.Spec.(empty)
    (fun () -> 
      Tools.list_data ();
      main ())
;;

(* Subcommand 'search STRING'. Searches for a string in our indexed
 * DNA sequence and returns the position. *)
let search =
  Command.basic ~summary:"List the CFNA data in ./_data"
    Command.Spec.(
      empty
      +> anon ("str" %: string)
    )
    (fun str () -> 
      let res = DNA.pattern_search str !_data in 
        match res with
        | None -> Tools.not_found (); main ();
        | Some (_, i) -> Tools.found_at i; main (); )
;;

(* To prevent empty subcommands to end the program *)
let return =
  Command.basic ~summary:"" Command.Spec.(empty) (fun () -> main ())
;;

_subcommands :=
    Command.group ~summary: ("")
      [ 
        "data", list_data; 
        "exit", exit_program; 
        "", return;
        "search", search
      ]
;;

(* Finally, we run Command.run *)
let () = Command.run ~version: (Tools.get_version ()) 
          ~build_info:"DNA" command

