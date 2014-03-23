module Text' = Text
open Batteries
module Text = Text'

open Error
open Ast

type unary = Ast.unary
type binary = Ast.binary

type v = Value.t
type 'a ty = 'a Value.ty

module Nullary = struct
  open Value

  type 'a op = {
    op: unit -> 'a;
    r: 'a ty;
    str: string;
  }

  let apply op = op.op () |> op.r.inj
end

module Unary = struct
  open Value

  type ('a,'b) op = {
    a:  'a ty;
    r:  'b ty;
    op: 'a -> 'b;
    str: string;
  }

  let minus = {
    op = (~-.);
    a  = number;
    r  = number;
    str = "-";
  }

  let base = {
    op = (fun id -> String.sub id 0 (String.rindex id '.'));
    a  = symbol;
    r  = symbol;
    str = "^";
  }

  let not = {
    op = not;
    a  = boolean;
    r  = boolean;
    str = "not "
  }

  let size = {
    op = Array.size;
    a  = array;
    r  = number;
    str = "->size";
  }

  let pop = {
    op = Array.pop;
    a  = array;
    r  = value;
    str = "->pop";
  }

  let apply t v = match t.a.pro v with
    | Some v -> Some (t.r.inj (t.op v))
    | None -> None

  let signature t =
    Printf.sprintf "%s: %s -> %s" t.str t.a.rep t.r.rep

  let to_string t = t.str

end

module Binary = struct
  open Value

  type ('a,'b,'c) op = {
    a1: 'a ty;
    a2: 'b ty;
    r : 'c ty;
    op: 'a -> 'b -> 'c;
    str: string;
  }

  let arith op rep = {
    op = op;
    a1 = number;
    a2 = number;
    r  = number;
    str = rep;
  }

  let bool op rep = {
    op = op;
    a1 = boolean;
    a2 = boolean;
    r  = boolean;
    str = rep;
  }

  let pred  t op rep = {
    op = op;
    a1 = t;
    a2 = t;
    r  = boolean;
    str = rep;
  }

  let sym op rep = {
    op = op;
    a1 = symbol;
    a2 = symbol;
    r  = symbol;
    str = rep;
  }

  let cat = {
    op = Array.cat;
    a1 = array;
    a2 = array;
    r  = array;
    str = "@"
  }

  let add = {
    op = Array.add;
    a1 = array;
    a2 = value;
    r  = unit;
    str = "add"
  }

  let minus = arith (-.) "-"
  let plus  = arith (+.) "+"
  let mul   = arith ( *. ) "*"
  let div   = arith ( /. ) "*"
  let pow   = arith ( ** ) "**"
  let and_  = bool (&&) "and"
  let or_   = bool (||) "or"
  let lt    = pred number (<) "<"
  let leq   = pred number (<=) "<="
  let gt    = pred number (>) ">"
  let geq   = pred number (>=) ">="
  let cast  = pred symbol Text.starts_with ":>?"
  let prefix = sym (fun x y -> x ^ "." ^ y) "::"

  let apply t v v' = match t.a1.pro v, t.a2.pro v' with
    | Some r, Some r' -> Some ((r,r') |> uncurry t.op |> t.r.inj)
    | _ -> None

  let signature t = Printf.sprintf "(%s): %s -> %s -> %s"
    t.str t.a1.rep t.a2.rep t.r.rep

  let to_string t = t.str
end

module type BinaryTransform = sig
  type r
  val f: ('a,'b,'c) Binary.op -> r
end

module type UnaryTransform = sig
  type r
  val f: ('a,'b) Unary.op -> r
end

module MakeUnary(Op:UnaryTransform) = struct
  module U = Unary
  let apply =
    let a = Op.f in function
      | UMinus -> a U.minus
      | Base   -> a U.base
      | Not    -> a U.not
      | Property "size" -> a U.size
      | Property "pop" -> a U.pop
      | Property prop -> failwith prop
end

module MakeBinary(Op:BinaryTransform) = struct
  module B = Binary
  let apply =
    let a = Op.f in function
      | Plus    -> a B.plus
      | Minus   -> a B.minus
      | Mul     -> a B.mul
      | Div     -> a B.div
      | Pow     -> a B.pow
      | And     -> a B.and_
      | Or      -> a B.or_
      | Less    -> a B.lt
      | LessEq  -> a B.leq
      | Great   -> a B.gt
      | GreatEq -> a B.geq
      | CanCast -> a B.cast
      | Prefix  -> a B.prefix
      | ArrayCat  -> a B.cat
      | Method "add"  -> a B.add
      | Method met -> failwith met
end

let unary =
  let module M = MakeUnary(struct
    type r = v -> v option
    let f = Unary.apply
  end) in M.apply

let binary =
  let module M = MakeBinary(struct
    type r = v -> v -> v option
    let f = Binary.apply
  end) in M.apply

let string_of_unary =
  let module M = MakeUnary(struct
    type r = string
    let f = Unary.to_string
  end) in M.apply

let string_of_binary =
  let module M = MakeBinary(struct
    type r = string
    let f = Binary.to_string
  end) in M.apply

module Sig = struct
  let unary =
    let module M = MakeUnary(struct
      type r = string
      let f = Unary.signature
    end) in M.apply

  let binary =
    let module M = MakeBinary(struct
      type r = string
      let f = Binary.signature
    end) in M.apply
end

open Value

let call at rt f = function
  | [None,v] -> begin match at.pro v with
      | Some a -> Some (f a |> rt.inj)
      | None   -> None
  end
  | _ -> failwith "call arity"
