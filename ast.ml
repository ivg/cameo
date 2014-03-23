type id = string
type text = string

type t =
  | Unit
  | Ref of t
  | Num of float
  | Bool of bool
  | Id   of id
  | Unary of unary * t
  | Binary of binary * t * t
  | Set of t * t
  | GSet of t * t
  | Block of t list
  | IfThenElse of t * t * t
  | ArrayGet of t * t
  | ArraySet of t * t * t
  | While of t * t
  | Abort of t
  | Fun of id * (id * t option) list * t
  | Apply of t * (id option * t) list
  | Array of t list
  | For of id * t * t
  | Seq of t * t * t
  | Eq of t * t
  | Break
  | Return of t
and unary  = UMinus | Base | Not | Property of id
and binary =
  | Plus | Minus | Mul | Div | Pow | And | Or | Less | LessEq
  | Great | GreatEq | CanCast | Prefix | ArrayCat
  | Method of id




















