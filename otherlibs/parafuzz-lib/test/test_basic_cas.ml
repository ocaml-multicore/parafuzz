module Crowbar = Parafuzz_lib.Crowbar

let test () = 
    let v = Atomic.make 1 in
    let cas_succedeed_for_d = ref false in
    let d = Domain.spawn(fun () -> cas_succedeed_for_d := Atomic.compare_and_set v 1 2) in
    let cas_succedeed =  Atomic.compare_and_set v 2 3 in
    Domain.join d;
    if not cas_succedeed then
(*         Crowbar.check ((not !cas_succedeed_for_d) && Atomic.get v=3) *) 
        Crowbar.check (!cas_succedeed_for_d && Atomic.get v=2)


let ()  = 
	Crowbar.(add_test ~name:"Atomic Compare and Set check" [Crowbar.const 1] (fun _ ->
		Parafuzz_lib.run test
	))
