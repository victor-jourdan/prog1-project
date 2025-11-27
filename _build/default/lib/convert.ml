let string_to_fresh_var : string -> Term.t = fun _ -> Term.fresh_var ()

let query : string Ast.Atom.t list -> Query.t * (unit -> unit) = fun lst ->
  let rec query_aux : string Ast.Atom.t list -> Query.t (* (unit -> unit)*) = function
    | [] -> True
    | [a] -> let f, arg = (Ast.Atom.convert string_to_fresh_var a) in Atom (f, arg)
    | [a;b] -> let f, arg1 = (Ast.Atom.convert string_to_fresh_var a) in let g, arg2 = (Ast.Atom.convert (fun x -> Term.int_var (int_of_string x)) b) in And (Atom (f, arg1), Atom (g, arg2))
    | a::q -> let f, arg = (Ast.Atom.convert string_to_fresh_var a) in And (Atom (f, arg), query_aux q)
  in
  let q = query_aux lst in
  let var_list = Query.varInQuery q in
  let final_list = List.sort_uniq Term.var_compare var_list in
  let pp_v : Format.formatter -> Term.var -> unit = fun fmt x -> Format.fprintf fmt "(%a|@,%a)" Term.pp_var x Term.pp (Term.var x) in
  (q, fun () -> Format.fprintf Format.std_formatter "binding :@,@[<hov 0>%a@]" (Format.pp_print_list pp_v) final_list)

let rule : string Ast.Rule.t -> Query.atom_to_query_t = fun _R -> fun f args ->
  match (Ast.Atom.convert string_to_fresh_var (fst _R)) with (g, arg2) ->
    try
      (* TOCHECK Not sure if this is relevant (1/2) : *)
      let s1 = Term.save () in

      let _ = Unify.unify (Term.make f args) (Term.make g arg2) in
      (* The unification succeed, hence : *)
      (*  *)
      let check_premisses = fst (query (snd _R)) in

      (* defining variable values : *)

      (* This is obligatory for otherwise toBindVar won't return the variables but the observable variables *)
      let s2 = Term.save () in
      let _ = Term.reset () in
      
      let toBindVar = Term.varInLst args in
      
      let _ = Term.restore s2 in
     
      let termEquation : Term.var -> Query.t = fun x -> match (Term.observe (Term.var x)) with
        | Var y -> Equals (Term.var x, Term.var y) 
        | Fun (f, arg) -> Equals (Term.var x, Term.make f arg) in

      let toBindVar = List.map termEquation toBindVar in
      
      let check_variables = (Query.bigAnd toBindVar) in

      (* TOCHECK Not sure if this is relevant (2/2) : *)  
      let _ = Term.restore s1 in

      And (check_variables, check_premisses)
    with
      | Unify.Unification_failure -> False

let rec rules : (string Ast.Rule.t) list -> Query.atom_to_query_t = function
  | [] -> fun _ _ -> False
  | r::q -> fun f args -> Or (rule r f args, rules q f args)
