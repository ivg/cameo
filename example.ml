type value =
  | Symbol of string
  | Number of int

type 'a t = {inj:'a -> value; pro: value -> 'a option}

let symbol = {
  inj = (fun s -> Symbol s);
  pro = function Symbol s -> Some s | _ -> None  
}

let number = {
  inj = (fun s -> Number s);
  pro = function Number s -> Some s | _ -> None  
}

type ops = {
  num: int -> int;
  sym: string -> string;
}

type v = {
  ip: 'a. 'a t
}

let sym = {
  ip = symbol;
}
let num = {
  ip = number;
}

let rec apply t v =
  match t.ip.pro v with
    | Some r -> t.ip.inj (t.op r)
    | None -> v
and run (t,t') = function
  | hd::tl -> apply t hd :: apply t' hd  :: run (t,t') tl
  | [] -> []

let s = Symbol "hello"
let n = Number 42

let _ = run (sym,num) [s; n; s; s; n]

(* let _ = apply symbol so s *)
(* let _ = apply number no n *)

let () = ()















