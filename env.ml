open Error

module Map = Map.Make(String)

type 'a t = 'a Map.t list
type id = string

let bindings stacks =
  List.map (fun s -> Map.bindings s) stacks |> List.concat

let to_string string_of_value env =
  let incr nest = "  " ^ nest in
  let rec inner nest =
    let string_of_frame f =
      Map.fold
        (fun id v str -> str ^ incr nest ^ id ^ "=" ^ (string_of_value v) ^ "\n") f "" in
    function
    | [] -> ""
    | hd::tl ->
      nest ^ "{\n" ^
      string_of_frame hd ^
      (inner (incr nest) tl) ^ nest ^ "}\n" in
  inner "" (List.rev env)

let rec get id = function
  | [] -> raise (Unbound_variable id)
  | dic :: dics ->
    try Map.find id dic with Not_found -> get id dics

let set id v = function
  | dic :: dics -> Map.add id v dic :: dics
  | [] -> failwith "empty stack in set_var"

let rec set_global id v env =
  let rec loop = function
    | d :: ds when Map.mem id d -> Map.add id v d :: ds
    | d :: ds -> d :: loop ds
    | [] -> raise (Unbound_variable id) in
  let hd = List.hd env in
  hd::(env |> List.tl |> loop)

let exists id env =
  try ignore(get id env); true with Unbound_variable _ -> false

let merge env env' =
  let union d d' = Map.fold Map.add d' d in
  let rec loop env env' = match env,env' with
    | hd::tl, hd'::tl' -> union hd hd' :: loop tl tl'
    | [],env | env,[] -> env in
  let env,env' = Pair.map List.rev (env,env') in
  List.rev (loop env env')

let empty = [Map.empty]

let pop = function
  | hd::tl -> tl
  | [] -> raise Empty_stack

let push env = Map.empty :: env






















