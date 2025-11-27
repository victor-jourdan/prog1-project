(* This is a Query *)
type t =
  | Atom of string * Term.t list
  | Equals of Term.t * Term.t
  | And of t * t
  | Or of t * t
  | False
  | True

(** Prints the Query *)
val pp : Format.formatter -> t -> unit

(** Turns an atom to a query (condering the infering rules) *)
type atom_to_query_t = string -> Term.t list -> t

(** Returns a conjonction of every query in the list *)
val bigAnd : t list -> t

(** Returns every variable appearing in a given query *)
val varInQuery : t -> Term.var list

(** Prints every binding where the Query is true *)
val search : ?atom_to_query:atom_to_query_t -> (unit -> unit) -> t -> unit

(** Tests if a query has a solution, uses search *)
val has_solution : ?atom_to_query:atom_to_query_t -> t -> bool