(** Le type [t] correspond à la représentation interne des termes.
  * Le type [var] représente les variables, c'est à dire les objets que
  * l'on peut instantier.
  * Le type [obs_t] correspond à un terme superficiellement explicité. *)
exception Unbound_Variable

type t
type var
type obs_t = Fun of string * t list | Var of var

(** Modification d'une variable. *)
val bind : var -> t -> unit

(** Remove the binding of a given variable *)
val remove : var -> unit

(** Observation d'un terme. *)
val observe : t -> obs_t

(** Egalité syntaxique entre termes et variables. *)

val equals : t -> t -> bool
val var_equals : var -> var -> bool


(** Return every variables in a given term *)
val varInLst : t list -> var list

val varIn : var list -> t -> var list



(** Constructeurs de termes. *)

(** Création d'un terme construit à partir d'un symbole
  * de fonction -- ou d'une constante, cas d'arité 0. *)
val make : string -> t list -> t

(** Création d'un terme restreint à une variable. *)
val var : var -> t

val int_var : int -> t

(** Création d'une variable fraîche. *)
val fresh : unit -> var

(** Combinaison des deux précédents. *)
val fresh_var : unit -> t

(** Manipulation de l'état: sauvegarde, restauration. *)

type state

(** [save ()] renvoie un descripteur de l'état actuel. *)
val save : unit -> state

(** [restore s] restaure les variables dans l'état décrit par [s]. *)
val restore : state -> unit

(** Remise à zéro de l'état interne du module.
    Aucun impact sur les termes déja créés, mais garantit que
    les futurs usages seront comme dans un module fraichement
    initialisé. *)
val reset : unit -> unit

(** Check if a variable is defined in binding *)

val isDefined : var -> bool

val var_compare : var -> var -> int

(** Pretty printing *)

val pp : Format.formatter -> t -> unit

val pp_var : Format.formatter -> var -> unit

val display_bind : unit -> unit