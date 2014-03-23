(** A variant type, containing values. *)

(** {4 Type definitions}  *)
type t                                  (** value *)
type arg = string option * t            (** function argument *)
type env = t Env.t                      (** environment  *)

(** A first class object, representing a type of value, that can be
    represented with a ocaml type ['a] *)
type 'a ty = {
  inj: 'a -> t;                (** injects value to variant  *)
  pro: t -> 'a option;         (** tries to project value    *)
  rep: string;               (** a human readable representation *)
}

(** {4 Predefined value types}  *)

val unit: unit ty
(** a singleton unit type *)

val number:  float  ty
(** a number, using a float as an underlying representation *)
val symbol:  string ty
(** symbolic value, represented as a string  *)
val boolean: bool   ty
(** boolean  *)
val value: t ty
val closure: (env -> arg list -> (env*t)) ty
(** type representing function type.

    Function type is an applicable object, accepting environment of
    evaluation and an argument list and returning modified
    environment coupled with the evaluated value. *)

val typename: t -> string

(** {4 Printing and misc functions}  *)
val to_string: t -> string
(** prints value  *)

module Array : sig
  type e = t
  type t
  val empty: unit -> t
  val copy: t -> t
  val create: e list -> t
  val of_seq: float -> float -> float -> t
  val size: t -> float
  val add: t -> e -> unit
  val pop: t -> e
  val set: t -> float -> e -> unit
  val get: t -> float -> e
  val cat: t -> t -> t
  val fold: ('a -> e -> 'a) -> 'a -> t -> 'a
  val to_list: t -> e list
end

val array: Array.t ty
