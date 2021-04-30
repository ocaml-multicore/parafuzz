let test () = 
    let open Domain in
    let m = Mutex.create () in
    Mutex.lock m;
    let d = spawn (fun () -> Mutex.lock m; Mutex.unlock m) in
    join d;
    Mutex.unlock m

let ()  = 
    Parafuzz_lib.run test 
