module Crowbar = Parafuzz_lib.Crowbar
open Domain

let thread1 c v m = 
  Mutex.lock m;
  Condition.wait c m;
  v := (!v) + 1;
  Mutex.unlock m

let thread2 c = 
  Condition.signal c


let test () = 
    let v = ref 1 in
    let m = Mutex.create () in
    let c = Condition.create () in 
    let d1 = Domain.spawn(fun () -> thread1 c v m) in
    let d2 = Domain.spawn(fun () -> thread2 c) in
    Domain.join d1;
    Domain.join d2;
    Crowbar.check (!v = 2)

let ()  = 
	Crowbar.(add_test ~name:"Conditional Variable check" [Crowbar.const 1] (fun _ ->
		Parafuzz_lib.run test
	))
