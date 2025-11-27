let term_tests = "Term", [ (* {{{ *)

  "Fresh vars not var_equals", `Quick, begin fun () ->
    assert (not (Term.(var_equals (fresh ()) (fresh ()))))
  end ;

  "Fresh vars not equals", `Quick, begin fun () ->
    assert (not (Term.(equals (var (fresh ())) (var (fresh ())))))
  end ;

  "Bind", `Quick, begin fun () ->
    Term.reset () ;
    let v = Term.fresh () in
    let t = Term.make "c" [] in
    Term.bind v t ;
    assert Term.(equals (var v) t)
  end ;

  "Restore", `Quick, begin fun () ->
    Term.reset () ;
    let v = Term.fresh () in
    let t = Term.make "c" [] in
    let s = Term.save () in
    assert (not Term.(equals (var v) t)) ;
    Term.bind v t ;
    assert Term.(equals (var v) t) ;
    Term.restore s ;
    assert (not Term.(equals (var v) t))
  end ;

  "Restore to non-empty stack", `Quick, begin fun () ->
    Term.reset () ;
    Term.bind (Term.fresh ()) (Term.make "c" []) ;
    let v = Term.fresh () in
    let t = Term.make "c" [] in
    let s = Term.save () in
    Term.bind v t ;
    Term.restore s ;
    assert (not Term.(equals (var v) t))
  end ;

  "Printing", `Quick, begin fun () ->
    Term.reset () ;
    let v0 = Term.fresh () in
    let v = Term.var v0 in
    Format.printf "%a@." Term.pp Term.(make "c" []) ;
    Format.printf "%a@." Term.pp Term.(make "c" [v]) ;
    Format.printf "%a@." Term.pp Term.(make "c" [v;v]) ;
    Term.bind v0 (Term.make "d" []) ;
    Format.printf "%a@." Term.pp Term.(make "c" [v]) ;
  end ;

] (* }}} *)

let unify_tests = "Unify", [ (* {{{ *)

    "f(X,a) = f(a,X)", `Quick, begin fun () ->
      let open Term in
      reset () ;
      let x = var (fresh ()) in
      let a = make "a" [] in
      let u = make "f" [x;a] in
      let v = make "f" [a;x] in
      assert (not (equals u v)) ;
      Unify.unify u v ;
      assert (equals u v)
    end ;
    
    "f(X,b) = f(a,X)", `Quick, begin fun () ->
      let open Term in
      reset () ;
      let x = var (fresh ()) in
      let a = make "a" [] in
      let b = make "b" [] in
      let u = make "f" [x;b] in
      let v = make "f" [a;x] in
      assert (not (equals u v)) ;
      Alcotest.check_raises "unify" Unify.Unification_failure
        (fun () -> Unify.unify u v)
    end ;

    "X = f(X)", `Quick, begin fun () ->
      let open Term in
      reset () ;
      let x = var (fresh ()) in
      let fx = make "f" [x] in
      Alcotest.check_raises "unify" Unify.Unification_failure
        (fun () -> Unify.unify x fx)
    end ;
    
    "X = f(Y)", `Quick, begin fun () ->
      let open Term in
      reset () ;
      let x = var (fresh ()) in
      let fy = make "f" [var (fresh ())] in
      Unify.unify x fy
    end ;
    (*
    "X = f^k(Y) with large k", `Quick, begin fun () ->
      let open Term in
      let rec f t n = if n = 0 then t else f (Term.make "f" [t]) (n-1) in
      reset () ;
      let x = var (fresh ()) in
      let fky = f (var (fresh ())) 100_000_000 in
      Unify.unify x fky
    end ;

    "X = f^k(X) with large k", `Quick, begin fun () ->
      let open Term in
      let rec f t n = if n = 0 then t else f (Term.make "f" [t]) (n-1) in
      reset () ;
      let x = var (fresh ()) in
      let fkx = f x 100_000_000 in
      Alcotest.check_raises "unify" Unify.Unification_failure
        (fun () -> Unify.unify x fkx)
    end ;

    "Zig", `Quick, begin fun () ->
      let node x = Term.make "n" [x; Term.make "l" []] in
      let rec comb t n =
        if n = 0 then t else comb (node t) (n-1)
      in
      let n = 50_000_000 in
      let t1 = comb (Term.fresh_var ()) n in
      let t2 = comb (Term.fresh_var ()) n in
      Unify.unify t1 t2
    end ;

    "Zag", `Quick, begin fun () ->
      let node x = Term.make "n" [Term.make "l" []; x] in
      let rec comb t n =
        if n = 0 then t else comb (node t) (n-1)
      in
      let n = 50_000_000 in
      let t1 = comb (Term.fresh_var ()) n in
      let t2 = comb (Term.fresh_var ()) n in
      Unify.unify t1 t2
    end ;

    "Zig-zag", `Quick, begin fun () ->
      let leaf = Term.make "l" [] in
      let node x =
        Term.make "n" [leaf; Term.make "n" [x; leaf]] in
      let rec zigzag t n =
        if n = 0 then t else zigzag (node t) (n-1)
      in
      let n = 10_000_000 in
      let t1 = zigzag (Term.fresh_var ()) n in
      let t2 = zigzag (Term.fresh_var ()) n in
      Unify.unify t1 t2
    end ;

    "Equalities", `Quick, begin fun () ->
      Term.reset () ;
      let x = Term.fresh_var () in
      let y = Term.fresh_var () in
      let f t = Term.make "f" [t] in
      Unify.unify x (f y)
    end*)

] (* }}} *)

let (==) a b = Query.Equals (a,b)
let (||) a b = Query.Or (a,b)
let (&&) a b = Query.And (a,b)

let queries = "Queries", let open Query in [ (* {{{ *)

  "True", `Quick, begin fun () ->
    assert (has_solution True)
  end ;

  "False", `Quick, begin fun () ->
    assert (not (has_solution False))
  end ;

  "Foo", `Quick, begin fun () ->
    assert (not (has_solution (Atom ("foo",[]))))
  end ;

  "Closed propositional", `Quick, begin fun () ->
    let q1 = And (False,True) in
    let q2 = Or (False,True) in
    assert (not (has_solution q1)) ;
    assert (not (has_solution (Or (q1,q1)))) ;
    assert (has_solution q2) ;
    assert (has_solution (Or (q1,q2)))
  end ;

  "Equalities", `Quick, begin fun () ->
    Term.reset () ;
    let x = Term.fresh_var () in
    let y = Term.fresh_var () in
    let f t = Term.make "f" [t] in

    let s = Term.save () in
    assert (has_solution (x == f y)) ;
    Term.restore s ;

    let s = Term.save () in
    assert (has_solution (y == x)) ;
    Term.restore s ;

    let s = Term.save () in
    assert (has_solution ((x == y || x == f y) && (x == f y))) ;
    Term.restore s ;

    assert (not (has_solution ((x == y || x == f y) &&
                               (x == f y && x == y))))
  end

] (* }}} *)

let () =
  Alcotest.run ~argv:Sys.argv "Infératrice"
    [ term_tests ;
      unify_tests ;
      queries ]