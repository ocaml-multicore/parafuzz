module Crowbar = Parafuzz_lib.Crowbar

let test () = 
    let v = Atomic.make 1 in
    let d = Domain.spawn(fun () -> Atomic.fetch_and_add v 2) in
    Atomic.fetch_and_add v 3;
    Domain.join d;
    Crowbar.check (Atomic.get v = 3)

let ()  = 
	Crowbar.(add_test ~name:"Atomic Fetch and Add check" [Crowbar.const 1] (fun _ ->
		Parafuzz_lib.run test
	))
