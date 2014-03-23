open Batteries
open Ast
open Result

open Value
open Error

exception Ret of (Value.t Env.t * Value.t)

let rec string_of_ast  = function
  | Unit -> "()"
  | Ref id -> "$"^ string_of_ast id
  | Num n  -> Printf.sprintf "%g" n
  | Bool b -> string_of_bool b
  | Id id -> id
  | Unary (op,e) -> Op.string_of_unary op ^ string_of_ast e
  | Binary (op,e,e') ->
      string_of_ast e ^ Op.string_of_binary op ^ string_of_ast e'
  | Set (id,v) -> string_of_ast id ^ " <- " ^ string_of_ast v
  | GSet (id,v) -> string_of_ast id ^ " <-- " ^ string_of_ast v
  | _ -> "<unknown>"

let array_error ar idx =
  let str = Printf.sprintf
    "cannot acces expression:\n%s[%s]\n as an array."
    (string_of_ast ar) (string_of_ast idx) in
  raise (Type_error str)

let operator_error ssig ops =
  let str = Printf.sprintf
    "cannot apply operator with signature\n%s\n to operand(s)\n%s"
    ssig ops in
  raise (Type_error str)

let type_error v ty =
  let str = Printf.sprintf
    "got value '%s', when expected value of type '%s'"
    (Value.to_string v) (ty.Value.rep) in
  raise (Type_error str)

let project ty v = match ty.pro v with
  | Some r -> r
  | None -> type_error v ty

let bool = project boolean
let none = unit.inj ()

let rec run env = function
  | Unit   -> env, none
  | Ref ide ->
      let env,id = run env ide in
      env, Env.get (project symbol id) env
  | Num n  -> env, number.inj  n
  | Id id  -> env, symbol.inj id
  | Bool b -> env, boolean.inj b
  | Set  (id,e) ->
      let env,id,v = run2 env id e in
      Env.set (project symbol id) v env, none
  | GSet (id,e) ->
      let env,id,v = run2 env id e in
      Env.set_global (project symbol id) v env, none
  | Unary (op,e) ->
      let env,v = run env e in env,
      begin match Op.unary op v with
        | Some v -> v
        | None -> operator_error (Op.Sig.unary op)
            (string_of_ast e ^ " : " ^ typename v)
      end
  | Binary (op,e,e') ->
      let env,v,v' = run2 env e e' in env,
      begin match Op.binary op v v' with
        | Some v -> v
        | None   ->
            let str = Printf.sprintf "``%s : %s'' and ``%s : %s''"
              (string_of_ast e ) (typename v )
              (string_of_ast e') (typename v') in
            operator_error  (Op.Sig.binary op) str
      end
  | Eq (v,v') ->
      let (env,r),(env',r') = run env v, run env v' in
      Env.merge env env', begin
        try boolean.inj (compare r r' = 0) with
          | Invalid_argument _ -> none
      end
  | Block exprs -> begin
    try
      let env = Env.push env in
      let env,res = List.fold_left
        (fun (env,res) expr ->
          if Break.has env then (env,res) else run env expr)
        (env,none) exprs in
      Env.pop env, res
    with Ret (env,r) -> raise (Ret (Env.pop env, r))
  end
  | IfThenElse (cond,e,e') ->
      let env,v = run env cond in
      if bool v then run env e else run env e'
  | While (cond,expr) ->
      let env,r = run env cond in
      if not (Break.has env) && bool r then
        let env,_ = run env expr in
        run env (While (cond,expr))
      else (Break.del env, r)
  | Break -> Break.set env, unit.inj ()
  | Ast.Abort e ->
      let _,str = run env e in
      raise (Abort (project symbol str))
  | Seq (f,s,t) ->
      let env,f,s = run2 env f s in
      let env,t = run env t in
      env, Value.Array.of_seq
        (project number f)
        (project number s)
        (project number t) |> array.inj
  | For (id,are,be) ->
      let env,arv = run env are in
      let ar = project array arv in
      let env = Env.push env in
      let env,r = Array.fold (fun (env,_) ae ->
        if Break.has env
        then env,none
        else
          let env = Env.set id ae env in
          run env be) (env,none) ar in
      env |> Break.del |> Env.pop, r
  | Ast.Array es ->
      let env,rvs = List.fold_left (fun (env,rvs) exp ->
        let env,r = run env exp in
        env, r::rvs) (env,[]) es in
      env, Value.Array.create (List.rev rvs) |> array.inj
  | ArrayGet (ae,eidx) ->
      let env,a,idx = run2 env ae eidx in
      env, Array.get (project array a) (project number idx)
  | ArraySet (ae,eidx,te) ->
      let env,a,idx = run2 env ae eidx in
      let env,v = run env te in
      Array.set (project array a) (project number idx) v;
      env, none
  | Return e -> raise (Ret (run env e))
  | Fun (id,pars,expr) ->
      let named_arg = function
        | Some id, v -> Some (id, v)
        | _ -> None in
      let unamed_arg = function
        | None,v -> Some v
        | _ -> None in
      let named_par = function
        | id, Some v -> Some (id,v)
        | _ -> None in
      let unamed_par = function
        | id, None -> Some id
        | _ -> None in
      let eval_par (pars,env) (id,e) =
        let env',r = run env e in
        (id,r)::pars, Env.merge env env' in
      let def_pars,env = pars |> List.filter_map named_par |>
          List.fold_left eval_par ([],env) in
      let add es env =
        List.fold_left (fun env (id,v) -> Env.set id v env) env es in
      let f env' args =
        let upars = List.filter_map unamed_par pars in
        let uargs = List.filter_map unamed_arg args in
        let nargs = List.filter_map named_arg args  in
        if not (List.length upars = List.length uargs) then
          raise (InvalidArguments id);
        let pargs = List.combine upars uargs  in
        let env = Env.merge env env' |>
            Env.push  |> add def_pars |> add nargs |> add pargs in
        let env,r = try (run env expr) with Ret r -> r in
        Env.pop env, r in
      let fc = closure.inj f in
      Env.set id fc env, fc
  | Apply (clos,args) ->
      let env,fn = run env clos in
      let clos = (match symbol.pro fn with
        | Some id -> project closure (Env.get id env)
        | None    -> project closure fn) in
      let env,args = List.fold_left begin fun (env,res) (id,e) ->
        let env',r = run env e in
        Env.merge env env', (id,r)::res
      end (env,[]) args in
      clos env (List.rev args)
and run2 env e e' =
  let env,v   = run env e  in
  let env,v'  = run env e' in
  env,v,v'




















