let ()  =
	Crowbar.(add_test ~name:"Basic Atomic check" [Crowbar.range 100] (fun i ->
		let v = Atomic.make i in
		Parafuzz_lib.run (fun () -> (Atomic.incr v;
			Crowbar.check ((Atomic.get v) = i+2))
		)
	))
