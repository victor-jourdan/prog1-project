let rec pp : Format.formatter -> t -> unit = fun fmt query -> match query with
        | False -> Format.fprintf fmt "False"
        | True -> Format.fprintf fmt "True"
        | Atom(pred,args) -> 
        Format.fprintf fmt "%s(@[<hov 2>[%a]@]" pred (Format.pp_print_list Term.pp ~(fun ()  -> Format.fprintf ",@," fmt) l 
        | And(qa,qb) -> (
                let priority q = 
                        match q with 
                        | Or(_,_) -> "(",")"
                        | _ -> "",""
                in
                let qaOpeningParenthese, qaClosingParenthese = priority qa in
                let qbOpeningParenthese, qbClosingParenthese = priority qb in
                Format.fprintf fmt "@[<hov 2>%s%a%s and @ %s%a%s@]" qaOpeningParenthese pp qa qaClosingParenthese qbOpeningParenthese pp qb qbClosingParenthese
        | Or(qa,qb) -> Format.fprintf fmt "(%a or @ %a)" pp qa pp qb 
        | Equals(ta,tb) -> Format.fprintf fmt "


