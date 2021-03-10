module M : Scheduler.AFLQueue = struct

    let queue = Dllist.create ()
    
    let range ?(min=0) n = Crowbar.sample_from_generator @@ Crowbar.range ~min n

    let is_empty () = Dllist.is_empty queue

    let enqueue _ = ignore @@ Dllist.add (fun () -> 
        Printf.printf "Hello world from enqueue ()\n") queue

    let dequeue () = 
        let n = range @@ Dllist.length queue in
        Dllist.remove_at_index queue n

end

let run f =
    let afl_queue = (module M : Scheduler.AFLQueue) in
    Scheduler.run afl_queue f

