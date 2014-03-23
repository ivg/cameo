(** Helper module to register operations in the environment. *)

module Nullary : sig
  val reg: 'a Op.Nullary.op -> Value.env -> Value.env
end

module Unary : sig
  val reg: ('a,'b) Op.Unary.op -> Value.env -> Value.env
end

module Binary : sig
  val reg: ('a,'b,'c) Op.Binary.op -> Value.env -> Value.env
end
