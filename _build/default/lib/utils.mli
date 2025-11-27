(** merge 2 lists into a list of couple *)
val merge_list : 'a list -> 'b list -> ('a * 'b) list

(***)
val linearize_list : ('a -> 'b) -> 'a list list -> 'b list