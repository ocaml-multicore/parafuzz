let ()  = 
    Parafuzz_lib.run (fun () -> Scheduler.context_switch ())
