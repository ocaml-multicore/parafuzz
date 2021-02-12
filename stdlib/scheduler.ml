effect Context_switch : unit
effect Fork           : (unit -> unit) -> unit

type 'a cont = ('a,unit) continuation
effect Suspend : ('a cont -> unit) -> 'a
effect Resume  : ('a cont * 'a) -> unit

let fork f = perform (Fork f)
let suspend f = perform (Suspend f)
let resume (k,v) = perform (Resume (k,v))
let context_switch () = perform Context_switch

let run main = 
    match main () with
    | () -> ()
    | effect Context_switch k -> ()
    | effect (Fork f) k -> ()
    | effect (Suspend f) k -> f k; ()
    | effect (Resume (k',v)) k -> ()

