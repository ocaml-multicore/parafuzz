module Crowbar = Parafuzz_lib.Crowbar

let test () = 
    let v = Atomic.make 1 in
    let val_in_d = ref 0 in
    let d = Domain.spawn(fun () -> val_in_d := Atomic.fetch_and_add v 2) in
    let val_in_main = Atomic.fetch_and_add v 3 in
    Domain.join d;
    Crowbar.check ((!val_in_d = 1 && val_in_main = 3 && Atomic.get v = 6) || (!val_in_d = 4 && val_in_main = 1 && Atomic.get v = 6))

let ()  = 
	Crowbar.(add_test ~name:"Atomic Fetch and Add check" [Crowbar.const 1] (fun _ ->
		Parafuzz_lib.run test
	))
