open Batteries
open Value
open Op
open Error


module Nullary = struct
  open Op.Nullary

  let call op env = function
    | [] -> env, apply op 
    | _  -> raise (InvalidArguments op.str)
  let reg op = Env.set op.str (closure.inj (call op))
end

module Unary = struct
  open Op.Unary

  let call op env =  function
    | [None, v] ->
        env, begin match apply op v with
          | Some r -> r
          | None ->
              let str = Printf.sprintf
                "cannot apply function %s to argument \
                 '%s' of type %s.  Expecting value of type %s"
                op.str (Value.to_string v) (Value.typename v)
                op.a.rep in
              raise (Type_error str)
        end
    | _ -> raise (InvalidArguments op.str)

  let reg op = Env.set op.str (closure.inj (call op))
end

module Binary = struct
  open Op.Binary

  let call op env = function
    | [None, v; None, v'] ->
        env, begin match apply op v v' with
          | Some r -> r
          | None ->
              let str = Printf.sprintf
                "cannot apply function '%s: %s -> %s -> %s'\
                 to arguments '%s' of type %s and '%s' of type %s"
                op.str op.a1.rep op.a2.rep op.r.rep
                (Value.to_string v) (Value.typename v)
                (Value.to_string v') (Value.typename v') in
              raise (Type_error str)
        end
    | _ -> raise (InvalidArguments op.str)

  let reg op = Env.set op.str (closure.inj (call op))
end




















