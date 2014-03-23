open Batteries

module Unary = struct
  open Op.Unary
  open Lib.Unary
  open Value

  let load_cmd env args =
    let env = List.fold_left (fun env (_,fname) ->
        let name = match symbol.pro fname with
          | Some str -> str
          | None -> invalid_arg "Expected filename" in
        let name = QString.unquote name in
        let name = if Filename.check_suffix name "cam"
          then name else name ^ ".cam" in
        let name = if Sys.file_exists name then name else
            let path =
              try Sys.getenv "CAMEO_PATH" with Not_found -> "" in
            Filename.concat path name in
        try
          let ic = open_in name in
          let lexbuf = Lexing.from_channel ic in
          Lexing.(lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with
                                         pos_fname = name});
          let prog = Parser.program Lexer.tokens lexbuf in
          List.fold_left (fun env stmt ->
            let env,v = Executor.run env stmt  in
            env) env prog
      with
      | Parser_error.T (msg,err_s,err_e) ->
        let err_s = Lexing.({err_s with pos_fname = name}) in
        let line = Parser_error.extract_line err_s in
        raise (Error.ProgramFailed
                 (Parser_error.string_of_exn line
                    (msg,err_s,err_e)))
      | Sys_error err ->
        let msg = Printf.sprintf "failed to load file «%s»: %s"
            name err in
        invalid_arg msg)
        env args in
    env, unit.inj ()


  let array_to ty str op = {
    op = op;
    a  = array;
    r  = ty;
    str = str;
  }

  let array_to_number = array_to number
  let array_to_unit   = array_to unit
  let array_to_value  = array_to value
  let array_to_array  = array_to array

  let shell  = {
    op = (fun cmd -> float (Sys.command (QString.unquote cmd)));
    a  = symbol;
    r  = number;
    str = "shell";
  }

  let arith str op = {
    op = op;
    a  = number;
    r  = number;
    str = str;
  }



  let abs = arith "abs" abs_float
  let sqrt = arith "sqrt" sqrt
  let log = arith "log" log
  let log10 = arith "log10" log10
  let cos = arith "cos" cos
  let sin = arith "sin" sin
  let tan = arith "tan" tan
  let acos = arith "acos" acos
  let asin = arith "asin" asin
  let atan = arith "atan" atan
  let cosh = arith "cosh" cosh
  let sinh = arith "sinh" sinh
  let tanh = arith "tanh" tanh
  let ceil = arith "ceil" ceil
  let floor = arith "floor" floor

  let size = array_to_number "size" Array.size
  let pop  = array_to_value  "pop"  Array.pop
  let copy = array_to_array  "copy" Array.copy


  let print output env args =
    let quote = ref false in
    let b = Buffer.create 64 in
    List.iter
      (function  (Some "q",_) -> quote := true;
               | (_, v) -> Buffer.add_string b (Value.to_string v))
      args;
    let s = if quote.contents
      then Printf.sprintf "\"%s\"" (Buffer.contents b)
      else Buffer.contents b in
    env, output s

  let load output env =
    let env = env
    |> reg size |> reg pop   |> reg copy
    |> reg log  |> reg log10 |> reg cos  |> reg sin
    |> reg tan  |> reg acos  |> reg asin |> reg atan
    |> reg cosh |> reg sinh  |> reg tanh |> reg ceil
    |> reg floor |> reg shell in
    let do_output str = unit.inj (output str) in
    let no_output str = symbol.inj str in
    let env = Env.set "print" (closure.inj (print do_output)) env in
    let env = Env.set "cat" (closure.inj  (print no_output)) env in
    let env = Env.set "echo" (closure.inj (print no_output)) env in
    Env.set "load" (closure.inj load_cmd) env
end


module Binary = struct
  open Op.Binary
  open Lib.Binary
  open Value

  let push = {
    a1  = array;
    a2  = value;
    r   = unit;
    op  = Array.add;
    str = "push";
  }

  let arith str op = {
    op = op;
    a1  = number;
    a2  = number;
    r  = number;
    str = str;
  }


  let atan2 = arith "atan2" atan2
  let fmod   = arith "mod" mod_float

  let load _ env =
    env |> reg push |> reg atan2 |> reg fmod
end


let load ?(output=(fun _ -> ())) env =
  env |> Unary.load output |> Binary.load output
