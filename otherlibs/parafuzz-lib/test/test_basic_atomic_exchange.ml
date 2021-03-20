module Crowbar = Parafuzz_lib.Crowbar

let test () = 
    let v = Atomic.make 1 in
    let v_in_d = ref 0 in
    let d = Domain.spawn(fun () -> v_in_d := Atomic.exchange v 2) in
    let v_in_main = Atomic.exchange v 3 in
    Domain.join d;
    Crowbar.check ((v_in_main = 2 && !v_in_d = 1 && Atomic.get v = 3) || (v_in_main = 1 && !v_in_d = 3 && Atomic.get v = 2 ))

let ()  = 
	Crowbar.(add_test ~name:"Atomic Exchange check" [Crowbar.const 1] (fun _ ->
		Parafuzz_lib.run test
	))
