module Crowbar = Parafuzz_lib.Crowbar

let test () = 
    let v = Atomic.make 1 in
    let d = Domain.spawn(fun () -> ignore(Atomic.exchange v 2)) in
    ignore(Atomic.exchange v 3);
    Domain.join d;
    Crowbar.check (Atomic.exchange v 4 = 2)

let ()  = 
	Crowbar.(add_test ~name:"Atomic Exchange check" [Crowbar.const 1] (fun _ ->
		Parafuzz_lib.run test
	))
