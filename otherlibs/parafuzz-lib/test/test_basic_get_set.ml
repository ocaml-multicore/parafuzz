module Crowbar = Parafuzz_lib.Crowbar

let test () = 
    let v = Atomic.make 1 in
    let dom = Domain.spawn(fun () -> Atomic.set v 2) in
    Atomic.set v 3;
    Domain.join dom;
    Crowbar.check (Atomic.get v = 3)

let ()  = 
	Crowbar.(add_test ~name:"Basic Atomic check" [Crowbar.const 1] (fun _ ->
		Parafuzz_lib.run test
	))
