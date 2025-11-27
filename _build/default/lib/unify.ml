exception Unification_failure

exception VarIsIn_True
exception VarIsIn_False

let varIsIn : Term.var -> Term.t -> bool = fun x t1 ->
  let rec varIsIn_tlrec : Term.var -> Term.obs_t list -> bool = fun x lst ->
    if lst = [] then
      raise VarIsIn_False
    else begin
      varIsIn_tlrec x
      begin
        Utils.linearize_list (fun x -> x)
        begin
          List.map
          begin fun (t1:Term.obs_t) -> match t1 with
            | Var a when Term.var_equals a x -> raise VarIsIn_True
            | Var _ -> []
            | Fun (_, arg) -> List.map (fun y -> Term.observe y) arg
          end
          lst
        end
      end
    end
  in
  try
    varIsIn_tlrec x [Term.observe t1]
  with
    | VarIsIn_True -> true
    | VarIsIn_False -> false

let unify : Term.t -> Term.t -> unit = fun t1 t2 ->
  let rec unify_tlrec : (Term.t * Term.t) list -> unit = fun lst ->
    if lst = [] then ()
    else begin
      unify_tlrec
      begin
        Utils.linearize_list (fun x -> x)
        begin
          List.map
          begin
            fun (t1, t2) -> match (Term.observe t1, Term.observe t2) with
              | (Var x, Var y) -> begin if not(Term.isDefined x) then
                                          Term.bind x t2
                                        else if not(Term.isDefined y) then
                                          Term.bind y t1
                                        else
                                          raise Unification_failure;
                                        []
                                  end 
              | (Fun _, Var x) when not(varIsIn x t1) -> if Term.isDefined x then raise Unification_failure else Term.bind x t1; []
              | (Var x, Fun _) when not(varIsIn x t2) -> if Term.isDefined x then raise Unification_failure else Term.bind x t2; []
              | (Fun (f, arg1), Fun (g, arg2)) when f = g -> Utils.merge_list arg1 arg2
              | _ -> raise Unification_failure
          end
          lst
        end
      end
    end
  in
  unify_tlrec [(t1, t2)]