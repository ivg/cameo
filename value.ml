open Batteries
open Result


type t =
  | Unit
  | Symbol of string
  | Number of float
  | Boolean of bool
  | Array of t DynArray.t
  | Closure of (env -> arg list -> (env * t))
and arg = string option * t
and env = t Env.t

type 'a ty = {
  inj: 'a -> t;
  pro: t -> 'a option;
  rep: string;
}

let rec to_string = function
  | Unit      -> "None"
  | Symbol id -> QString.unquote id
  | Number n  -> Printf.sprintf "%g" n
  | Boolean b -> string_of_bool b
  | Closure _ -> "<fun>"
  | Array e ->
      let _,str = DynArray.fold_left
        (fun (sep,str) v -> ", ", str ^ sep ^ to_string v) ("","") e
      in "[" ^ str ^ "]"

let unit = {
  inj=(fun () -> Unit );
  pro=(function Unit -> Some () | _ -> None);
  rep="unit";
}

let number = {
  inj = (fun v -> Number v);
  pro = (function Number v -> Some v | _ -> None);
  rep = "number";
}

let symbol = {
  inj=(fun n -> Symbol n);
  pro=(function Symbol n -> Some n | _ -> None);
  rep="symbol";
}

let boolean = {
  inj=(fun n -> Boolean n);
  pro=(function Boolean n -> Some n | _ -> None);
  rep="boolean";
}

let closure = {
  inj=(fun f -> Closure f);
  pro=(function Closure f -> Some f | _ -> None );
  rep="<fun>";
}

let value = {
  inj=(fun v -> v);
  pro=(fun v -> Some v);
  rep="value";
}


module Array = struct
  type e = t
  type t = e DynArray.t
  open DynArray
  let copy = copy
  let empty = create
  let create = of_list
  let to_list = to_list
  let size  = float_of_int % length
  let add = add
  let set a i e = set a (int_of_float i) e
  let get a i = get a (int_of_float i)
  let pop a = let elem = last a in delete_last a; elem
  let cat a a' =
    let dst = copy a' in
    append a dst;
    dst

  let of_seq f s t =
    let n = 1 + int_of_float ((t -. f) /. s) in
    init n (fun i -> f +. (float_of_int i) *. s |> number.inj)

  let fold = fold_left
end

let array = {
  inj=(fun a -> Array a);
  pro=(function Array a -> Some a | _ -> None);
  rep="array";
}

let typename = function
  | Symbol _ -> symbol.rep
  | Number _ -> number.rep
  | Boolean _ -> boolean.rep
  | Closure _ -> closure.rep
  | Unit      -> unit.rep
  | Array _ -> array.rep
