(** Working with breaks

    Break is a special mark indicating a user request to break
    current computation. There is only one such mark per program.
  *)

val init: Value.t Env.t -> Value.t Env.t

val set: Value.t Env.t -> Value.t Env.t
(** [set_break env] returns a copy of [env] with enabled break
    mark. *)
val del: Value.t Env.t -> Value.t Env.t
(** [del_break env] is a copy of [env] with disabled break mark. *)
val has: Value.t Env.t -> bool
(** [has_break env] checks whether [env] contains enabled break
    mark *)
