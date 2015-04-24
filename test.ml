(* A better signature for a binary tree, avoiding the comparison function
 * found in motivation.ml. *)
module type BINTREE =
sig
  exception EmptyTree
  exception NodeNotFound

  type elt
  type tree

  val empty : tree
  val search : elt -> tree -> bool
  val insert : elt -> tree -> tree

end

module type COMPARABLE =
sig
  type t
  val compare : t -> t -> order
  val to_string : t -> string

  val generate: unit -> t
  val generate_gt: t -> unit -> t
  val generate_lt: t -> unit -> t
  val generate_between: t -> t -> unit -> t option
end



module BinSTree(C : COMPARABLE) : BINTREE with type elt = C.t =
struct
  (* Inside of here, you can use C.t to refer to the type defined in
   * the C module (which matches the COMPARABLE signature), and
   * C.compare to access the function which compares elements of type
   * C.t
   *)
  exception EmptyTree
  exception NodeNotFound

  (* Grab the type of the tree from the module C that's passed in
   * this is the only place you explicitly need to use C.t; you
   * should use elt everywhere else *)
  type elt = C.t

  (* One possible type for a tree *)
  type tree = Leaf | Branch of tree * elt list * tree

  (* Representation of the empty tree *)
  let empty = Leaf


  let rec insert (x : elt) (t : tree) : tree = 
    match t with
    | Leaf -> Branch (empty, [x], empty)
    | Branch (l, lst, r) -> 
      match lst with
      | [] -> failwith "Invalid tree: empty list as node"
      | hd::tl -> 
        match C.compare x hd with
        | Less -> Branch (insert x l, lst, r)
        | Greater -> Branch (l, lst, insert x r)
        | Equal -> Branch (l, [x]@[hd]@tl, r)


  let rec search (x : elt) (t : tree) : bool = 
    match t with
    | Leaf -> false
    | Branch (l, lst, r) -> 
      match lst with
      | [] -> failwith "Invalid tree: empty list as node"
      | hd::tl -> 
        match C.compare x hd with
        | Less -> search x l
        | Greater -> search x r
        | Equal -> if x = hd then true else search x (Branch(empty, tl, empty))



end