module type AFLQueue = sig
    val enqueue  : (unit -> unit) -> unit
    val dequeue  : unit -> (unit -> unit)
    val is_empty : unit -> bool
    val range    : ?min:int -> int -> int

end

effect Context_switch : unit
effect Fork           : (unit -> unit) -> int

type 'a cont = ('a,unit) continuation
effect Suspend : (('a cont * int) -> unit) -> 'a
effect Resume  : ('a cont * 'a * int) -> unit
effect Resume_without_context_switch  : ('a cont * 'a * int) -> unit
effect Range : (int * int) -> int
effect Id : int

let fork f = perform (Fork f)
let suspend f = perform (Suspend f)
let resume (k,v, id) = perform (Resume (k,v,id))
let resume_without_context_switch (k,v, id) = perform (Resume_without_context_switch (k,v,id))
let context_switch () = perform Context_switch
let get_current_domain_id () = perform Id

let range ?(min=0) n = perform (Range (min,n))

let run afl_module main =
    let current_id = ref 0 in
    let next_id = ref 1 in
    let domains_finished = ref 0 in

    let check_for_deadlock () = if (!next_id - !domains_finished) = 0 then () else failwith "Deadlock!"  in
    let module M = (val afl_module : AFLQueue) in
    let enqueue id k v = M.enqueue (fun () -> current_id := id; continue k v) in
    let dequeue () = if M.is_empty () then check_for_deadlock () 
        else M.dequeue () () 
    in
    let rec spawn f = 
        match f () with
        | () -> incr domains_finished; dequeue ()
        | effect Context_switch k -> enqueue !current_id k () ; dequeue () 
        | effect (Fork f) k -> 
            enqueue !current_id k (!next_id); 
            current_id := !next_id;  
            incr next_id;
            spawn f
        | effect (Suspend f) k -> f (k,!current_id); dequeue ()
        | effect (Resume (k',v, id)) k -> enqueue id k' v; enqueue !current_id k (); dequeue ()
        | effect (Resume_without_context_switch (k',v, id)) k -> enqueue id k' v; continue k ()
        | effect (Range (min, n)) k -> continue k @@ M.range ~min n
        | effect (Id) k -> continue k (!current_id)
    in
    spawn main

