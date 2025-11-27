let rec merge_list : 'a list -> 'b list -> ('a * 'b) list = fun l1 l2 -> match (l1, l2) with
  | ([], []) -> []
  | (h1::t1, h2::t2) -> (h1,h2)::(merge_list t1 t2)
  | _ -> failwith "Lists don't have the same size"

let linearize_list : ('a -> 'b) -> 'a list list -> 'b list = fun foo lst ->
  let rec linearize_lst_tlrec = fun f acc -> function
    | [] -> acc
    | []::q -> linearize_lst_tlrec f acc q
    | (x::q1)::q2 -> linearize_lst_tlrec f ((f x)::acc) (q1::q2)
  in
  linearize_lst_tlrec foo [] lst
