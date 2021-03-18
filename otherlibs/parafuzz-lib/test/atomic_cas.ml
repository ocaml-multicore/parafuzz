module Crowbar = Parafuzz_lib.Crowbar

let test () = 
    let v = Atomic.make 1 in
    let d = Domain.spawn(fun () -> ignore(Atomic.compare_and_set v 1 2)) in
    ignore(Atomic.compare_and_set v 1 3);
    Domain.join d;
    Crowbar.check (Atomic.get v = 3)

let ()  = 
	Crowbar.(add_test ~name:"Atomic Compare and Set check" [Crowbar.const 1] (fun _ ->
		Parafuzz_lib.run test
	))
