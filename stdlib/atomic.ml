type 'a t

external make : 'a -> 'a t = "%makemutable"
external _get : 'a t -> 'a = "%atomic_load"
external _exchange : 'a t -> 'a -> 'a = "%atomic_exchange"
external _compare_and_set : 'a t -> 'a -> 'a -> bool = "%atomic_cas"
external _fetch_and_add : int t -> int -> int = "%atomic_fetch_add"

let get r = 
    Scheduler.context_switch ();
    let _ = if true then () else () in
    _get r

let exchange r x = 
    Scheduler.context_switch ();
    let _ = if true then () else () in
    _exchange r x

let compare_and_set r x y = 
    Scheduler.context_switch (); 
    let _ = if true then () else () in
    _compare_and_set r x y

let fetch_and_add r x = 
    Scheduler.context_switch ();
    let _ = if true then () else () in
    _fetch_and_add r x

let set r x =
  exchange r x |> ignore
let incr r =
  fetch_and_add r 1 |> ignore
let decr r =
  fetch_and_add r (-1) |> ignore
