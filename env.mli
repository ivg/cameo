(** Program enviroment.

    A stack of frames, where each frame is dictionary of variables
    mapping each identifier of type [id] to single value of type
    ['a].
*)

(** {4 Types Definitions}  *)

type 'a t                               (** the environment  *)
type id = string                        (** variable id  *)

(** {4 Creating}  *)
val empty: 'a t
(** [empty] creates empty environment *)

(** {4 Accessing variables}  *)
val get: id -> 'a t -> 'a
(** [get id env] lookups a value of the variable [id]. Throws
    [Error.Unbound_variable] if no such variable exists. *)
val set: id -> 'a -> 'a t -> 'a t
(** [set id v env] returns a copy of [env] except that the variable
    with [id] is set to have value [v] if it exists. Otherwise a new
    variable is created. *)
val set_global: id -> 'a -> 'a t -> 'a t
(** [set_global id v env] lookups a variable with [id] in the
    outermost frames and sets its value to [v]. If no such variable
    exists raises [Error.Unbound_variable].  *)
val exists: id -> 'a t -> bool
(** [exists id env] evaluates to true if [env ] contains a variable
    with [id] *)

(** {4 Manipulating Environments}  *)
val merge: 'a t -> 'a t -> 'a t
(** [merge env env'] takes two environments with equal deepness
    (amount of frames) and merges them in a such way, that the
    resulting environment contains a union of variables from either
    environments and variables of [env'] have precedence over
    variables of [env] in a case of conflict.  *)
val pop: 'a t -> 'a t
(** [pop env] returns an environment with one frame removed from a
    top.  *)
val push: 'a t -> 'a t
(** [push env] returns an environment with an empty frame pushed on
    atop of [env]  *)

val to_string: ('a -> string) -> 'a t -> string

val bindings: 'a t -> (id * 'a) list
