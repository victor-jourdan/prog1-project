type t =
  | Atom of string * Term.t list
  | Equals of Term.t * Term.t
  | And of t * t
  | Or of t * t
  | False
  | True

type atom_to_query_t = string -> Term.t list -> t

let rec pp : Format.formatter -> t -> unit = fun fmt query -> match query with
        | False -> Format.fprintf fmt "False"
        | True -> Format.fprintf fmt "True"
        | Atom(pred,args) -> 
        Format.fprintf fmt "%s(@[<hov 2>[%a]@]" pred (Format.pp_print_list Term.pp ~pp_sep:(fun fmt ()  -> Format.fprintf fmt ",@," )) args 
        | And(qa,qb) -> (
                let priority q = 
                        match q with 
                        | Or(_,_) -> "(",")"
                        | _ -> "",""
                in
                let qaOpeningParenthese, qaClosingParenthese = priority qa in
                let qbOpeningParenthese, qbClosingParenthese = priority qb in
                Format.fprintf fmt "@[<hov 2>%s%a%s and @ %s%a%s@]" qaOpeningParenthese pp qa qaClosingParenthese qbOpeningParenthese pp qb qbClosingParenthese
        )
        | Or(qa,qb) -> Format.fprintf fmt "(%a or @ %a)" pp qa pp qb 
        | Equals(ta,tb) -> Format.fprintf fmt "@[<v 0>[%a @,==@ %a]@]" Term.pp ta Term.pp tb

let rec bigAnd : t list -> t = function
  | [] -> True
  | [a] -> a
  | [a;b] -> And (a, b)
  | a::q -> And (a, bigAnd q)

let varInQuery : t -> Term.var list = fun t1 ->
  let rec varInQuery_tlrec : Term.t list list -> t list -> Term.var list = fun acc lst ->
    if lst = [] then begin
      Term.varInLst (Utils.linearize_list (fun x -> x) acc)
    end
    else begin
      let n_acc = ref acc in
      let n_lst =
      begin
        Utils.linearize_list (fun x -> x)
        begin
          List.map
          begin
            function
              | Atom (_, arg) -> n_acc := arg::(!n_acc); []
              | Equals (t1, t2) -> n_acc := [t1;t2]::(!n_acc); []
              | And (q1, q2) -> [q1; q2]
              | Or (q1, q2) -> [q1; q2]
              | False -> []
              | True -> []
          end
          lst
        end
      end
      in
      varInQuery_tlrec (!n_acc) n_lst
    end
  in
  varInQuery_tlrec [] [t1]

let rec search : ?atom_to_query:atom_to_query_t -> (unit -> unit) -> t -> unit =
  fun ?(atom_to_query = fun _ _ -> False) display -> function
  | Atom (f, arg) -> search ~atom_to_query:atom_to_query display (atom_to_query f arg) (* The function atom_to_query makes a step in the tree*)
  | And (t1, t2) -> search ~atom_to_query:atom_to_query (fun () -> search ~atom_to_query:atom_to_query display t2) t1 (*We explore the tree of t2 then the tree of t1*)
  | Or (t1, t2) -> (* We'll look for the bindings that satisfy t1, then the others that satisfy t2 *)
                  let s = Term.save () in (* just in case *)
                   search ~atom_to_query:atom_to_query display t1; (* delete binding of var in t2*)
                   (* if the LHS cannot be evaluated to True *)
                   Term.restore s;
                   search ~atom_to_query:atom_to_query display t2; (* delete binding of var in t1*)
  | Equals (t1, t2) -> begin match (Term.observe t1, Term.observe t2) with
      | (Var x, Var y) when x = y -> display () (* a variable equals to itself (we match the observations) *)
      | (Var x, Var _) -> Term.bind x t2; display () (* x is still free in the binding because it's an observation *)
      | (Fun (f, arg1), Fun (g, arg2)) when f = g -> (* Fun (f, arg1) equals Fun (g, arg2) if and only if f = g and arg1 = arg2 *)
                                                      let couple_args = Utils.merge_list arg1 arg2 in
                                                     let couple_args = List.map (fun (u1, u2) -> Equals (u1, u2)) couple_args in
                                                     search ~atom_to_query:atom_to_query display (bigAnd couple_args) 
      | (Fun (f, arg), Var x) | (Var x, Fun (f, arg)) when not(Unify.varIsIn x (Term.make f arg)) -> (*We can bind x only if it doesn't appear in arg *)
                                                                        Term.bind x (Term.make f arg); display ()
      | _ -> ()
    end
  | False -> ()
  | True -> display () (* display the current correct instaciation *)

exception Model_found

let has_solution : ?atom_to_query:atom_to_query_t -> t -> bool = fun ?(atom_to_query = fun _ _ -> False) -> fun t1 -> 
 try
  search ~atom_to_query:atom_to_query (fun () -> raise Model_found) t1; (*We end search and raise Model_found as soon as we find a binding *)
  false
 with
  | Model_found -> true (*When Model_found is raised, it means the query has a solution (we found one in search) *)
