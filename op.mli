(** Generic operations.

    Type safe application of operation. Each operation has a
    signature and a realization.
*)


(** {6 Type definitions}  *)

type unary  = Ast.unary
(** a set of unary operators  *)

type binary = Ast.binary
(** a set of binary operators  *)

type v = Value.t
(** operand  *)

type 'a ty = 'a Value.ty


(** {4 Operations}

    Each operation takes corresponding number of operands and tries to
    apply them to a given operation. If operands do not conform to
    operation signature the value is not computed and application
    evaluates to [None].
*)
val unary:  unary -> v -> v option
(** [unary op v] tries to perform unary operation [op] on operand
    [v]. *)

val binary: binary -> v -> v -> v option
(** [binary op v v'] tries to perform binary operation [op] on
    operands [v] and [v']. *)


module Nullary : sig
  type 'a op = {
    op: unit -> 'a;
    r: 'a ty;
    str: string;
  }

  val apply: 'a op -> v
end

module Unary : sig
  type ('a,'b) op = {
    a:  'a ty;
    r:  'b ty;
    op: 'a -> 'b;
    str: string;
  }
  val apply: ('a,'b) op -> v -> v option
end

module Binary : sig
  type ('a,'b,'c) op = {
    a1: 'a ty;
    a2: 'b ty;
    r : 'c ty;
    op: 'a -> 'b -> 'c;
    str: string;
  }
  val apply: ('a,'b,'c) op -> v -> v -> v option
end

(** {4 Printing and Misc}  *)

val string_of_unary: unary -> string
val string_of_binary: binary -> string

(** Printable operation signatures  *)
module Sig : sig
  val unary: unary -> string
  val binary: binary -> string
end




















