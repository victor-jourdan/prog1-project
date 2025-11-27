  exception Unbound_Variable

  type var = int
  type t = Fun of string * t list | Var of var | Undefined
  type obs_t = Fun of string * t list | Var of var
  
  type state = {mutable content: t array; mutable size: int} 
  
  let binding = {content = Array.make 1 Undefined; size = 0}

  let isDefined : var -> bool = fun x -> match binding.content.(x) with
    | Undefined -> false
    | _ -> true

  let rec observe_find : var list -> t -> t = fun variables -> function
    | Undefined -> Undefined
    | Var a -> if isDefined a then
                  observe_find (a::variables) binding.content.(a)
               else begin
                  List.iter (fun x -> bind x (Var a)) variables;
                  Var a
               end
    | Fun (f, arg) -> List.iter (fun x -> bind x (Fun (f, arg))) variables; Fun (f,arg)
  
  and bind : var -> t -> unit = fun x v ->
    if x < binding.size then
      let n_v = observe_find [] v in
      binding.content.(x) <- n_v
    else
      raise Unbound_Variable


  let remove : var -> unit = fun x -> binding.content.(x) <- Undefined
  
  (** [save ()] renvoie un descripteur de l'état actuel. *)
  let save : unit -> state = fun () -> {content = Array.copy binding.content; size = binding.size}
    
  (** [restore s] restaure les variables dans l'état décrit par [s]. *)
  let restore : state -> unit = fun s -> binding.content <- Array.copy s.content; binding.size <- s.size
    
  (** Remise à zéro de l'état interne du module *)
  let reset : unit -> unit = fun () -> binding.content <- (Array.make 1 Undefined); binding.size <- 0
    
  (** Observation d'un terme. *)
  
  let observe : t -> obs_t = fun t1 -> match observe_find [] t1 with
    | Undefined -> raise Unbound_Variable
    | Fun (f, arg) -> Fun (f, arg)
    | Var x -> Var x
  
  (** Création d'un terme restreint à une variable. *)
  let var : var -> t = fun x -> Var x
  
  (** Création d'un terme restreint à une variable, à partir d'un string (voir convert.ml pour la justification) *)
  let int_var : int -> t = fun x -> Var x
    
  (** Création d'une variable fraîche. *)
  let fresh : unit -> var = fun () ->
    binding.size <- binding.size + 1;
    if Array.length binding.content < binding.size then begin
      let tmp_content = Array.copy binding.content in
      binding.content <- (Array.make (2 * binding.size) Undefined);
      for i = 0 to (binding.size - 2) do
        binding.content.(i) <- tmp_content.(i)
      done;
    end;
    (binding.size - 1)
    
  (** Combinaison des deux précédents. *)
  let fresh_var : unit -> t = fun () -> var (fresh ())

  (** Egalité syntaxique entre termes et variables. *)
  
  let var_equals : var -> var -> bool = fun x y -> x = y

  exception Equals_True
  exception Equals_False

  let equals : t -> t -> bool = fun t1 t2 ->
    let rec equals_tlrec : (obs_t * obs_t) list -> 'a = fun lst ->
      if lst = [] then
        raise Equals_True
      else begin
        equals_tlrec
        begin
          Utils.linearize_list (fun (x, y) -> (observe x, observe y))
          begin
            List.map
            begin function
              | (Var x, Var y) when var_equals x y -> raise Equals_True
              | (Fun (f, arg1), Fun (g, arg2)) when f = g -> Utils.merge_list arg1 arg2
              | _ -> raise Equals_False
            end
            lst
          end
        end
      end
    in
    try
      equals_tlrec [(observe t1, observe t2)]
    with
      | Equals_True -> true
      | Equals_False -> false
    
    let varInLst : t list -> var list = fun lst ->
      let rec varIn_tlrec : obs_t list -> var list = fun lst ->
        let passed = ref false in
        let n_lst =
        begin
          Utils.linearize_list (fun x -> x)
          begin
            List.map
            begin fun t1 -> match t1 with
              | Var x -> [Var x]
              | Fun (_, arg) -> passed := true; List.map (fun x -> observe x) arg
            end
            lst
          end
        end
        in
        if !passed then
          varIn_tlrec n_lst
        else begin
          let rec varIn_aux : var list -> obs_t list -> var list = fun acc -> function
            | [] -> acc
            | (Fun _)::q -> varIn_aux acc q
            | (Var x)::q -> varIn_aux (x::acc) q
          in
          varIn_aux [] lst
        end
      in
      varIn_tlrec (List.map (fun x -> observe x) lst)

  let varIn : var list -> t -> var list = fun lst t1 -> varInLst (Utils.linearize_list (fun x -> x) [[t1]; List.map (fun x -> var x) lst])
      (*FIXME Aller voir les appels à varIn dans varInQuery *)

  let var_compare : var -> var -> int = fun x y -> x - y 
    
  (** Constructeurs de termes. *)
    
  (** Création d'un terme construit à partir d'un symbole de fonction -- ou d'une constante, cas d'arité 0. *)
  let make : string -> t list -> t = fun f arg -> Fun (f, arg)  
  
  let pp : Format.formatter -> t -> unit = fun fmt tree ->
    let rec aux_pp_tree n fmt tree = match observe tree with
            | Var a -> Format.fprintf fmt "%s{%d}" (if n = 0 then "-" else "\\") (let varToInt : var -> int = fun w -> w in varToInt a)
            | Fun (a,l) -> let str = (if n = 0 then "-" else "\\") in Format.fprintf fmt "%s{%s}-@[<v 0>%a@]" str a (aux_pp_tree_list 0) l
    and aux_pp_tree_list n fmt l  = match l with
    | [] -> Format.fprintf fmt ""
    | h::t -> Format.fprintf fmt "@[<v 0>%a@,%a@]" (aux_pp_tree n) h (aux_pp_tree_list (n+1)) t
    in
    aux_pp_tree  0 fmt tree

let pp_var : Format.formatter -> var -> unit = fun fmt x -> Format.fprintf fmt "%d" x

let display_bind () =
  for i = 0 to (binding.size-1) do
    pp Format.std_formatter (binding.content.(i))
  done;
