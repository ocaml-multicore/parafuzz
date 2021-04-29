module Crowbar = Parafuzz_lib.Crowbar
open Domain

let thread1 m v =
  Mutex.lock m;
  v := (!v) + 1;
  Mutex.unlock m

let thread2 m v = 
  Mutex.lock m;
  v := (!v) + 1


let test () = 
    let v = ref 1 in
    let m = Mutex.create () in
    let d1 = Domain.spawn(fun () -> thread2 m v) in
    let d2 = Domain.spawn(fun () -> thread1 m v) in
    Domain.join d1;
    Domain.join d2;
    Crowbar.check (!v = 3)

let ()  = 
	Crowbar.(add_test ~name:"Mutex starvation check" [Crowbar.const 1] (fun _ ->
		Parafuzz_lib.run test
	))
