open Batteries
open Value


let disabled,enabled = Pair.map boolean.inj (false,true)
let break_id = "__break"

let init = Env.set break_id disabled
let set = Env.set_global break_id enabled
let del = Env.set_global break_id disabled
let has env = Env.get break_id env = enabled
