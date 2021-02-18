module type AFLQueue = sig
    val enqueue : (unit -> unit) -> unit
    val dequeue : unit -> (unit -> unit)
end

effect Context_switch : unit
effect Fork           : (unit -> unit) -> unit

type 'a cont = ('a,unit) continuation
effect Suspend : ('a cont -> unit) -> 'a
effect Resume  : ('a cont * 'a) -> unit

let fork f = perform (Fork f)
let suspend f = perform (Suspend f)
let resume (k,v) = perform (Resume (k,v))
let context_switch () = perform Context_switch

let run afl_module main =
    let module M = (val afl_module : AFLQueue) in
(*     let enqueue k v = M.enqueue (fun () -> continue k v) in *)
    let dequeue () = M.dequeue () () in
    let spawn f = 
        match f () with
        | () -> dequeue ()
       (* | effect Context_switch k -> enqueue k (); dequeue () 
        | effect (Fork f) k -> enqueue k (); spawn f
        | effect (Suspend f) k -> f k; dequeue ()
        | effect (Resume (k',v)) k ->
                enqueue k' v; enqueue k (); dequeue ()*)
    in
    spawn main

