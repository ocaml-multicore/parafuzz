module Crowbar = Parafuzz_lib.Crowbar

let test () = 
    let v = Atomic.make 1 in
    let d = Domain.spawn(fun () -> ignore @@ Atomic.fetch_and_add v 2) in
    ignore @@ Atomic.fetch_and_add v 3;
    Domain.join d;
    Printf.printf "v = %d\n%!" @@ Atomic.get v;
    Crowbar.check (Atomic.get v = 6)

let ()  = 
	Crowbar.(add_test ~name:"Atomic Fetch and Add check" [Crowbar.const 1] (fun _ ->
		Parafuzz_lib.run test
	))
