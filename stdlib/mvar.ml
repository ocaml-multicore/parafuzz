
type 'a state = 
| Filled of 'a * (('a * (unit Scheduler.cont)) Queue.t)
(* The first element in the pair represents the value stored.
	The second element contains a queue of continuations that have `put`, that are waiting to be run *)

| Empty of 'a Scheduler.cont Queue.t
(* This represents a queue of continuations that have `get`, waiting to be run. These will be run
	once the MVar has some value. *)

type 'a t = 'a state ref

let make_empty () = ref (Empty(Queue.create ()))

let make v = ref (Filled(v, (Queue.create ())))

let get mvar = match (!mvar) with
| Empty (wait_q) -> Scheduler.suspend (fun (cont,id) -> Queue.push (cont,id) wait_q)
| Filled (value, wait_q) -> 
	if Queue.is_empty wait_q then 
		(mvar := Empty(Queue.create ()); value)
	else 
		(let (waiting_f, nvalue, id) = Queue.pop wait_q in
		(mvar := Filled(nvalue, wait_q); 
		Scheduler.resume (waiting_f, (), id);
			value))    

let put v mvar = match (!mvar) with
| Empty (wait_q) -> 
	if Queue.is_empty wait_q then 
		mvar := Filled(v, Queue.create ()) 
	else 
		let (waiting_f,id) = Queue.pop wait_q in 
		Scheduler.resume (waiting_f, v, id)
| Filled (value, wait_q) -> Scheduler.suspend (fun (cont,id) -> Queue.push (cont, v, id) wait_q)

let put_all v mvar = match (!mvar) with
| Empty (wait_q) -> 
	if Queue.is_empty wait_q then 
		mvar := Filled(v, Queue.create ()) 
	else 
		Queue.iter (fun (cont, id) -> Scheduler.resume (cont, v, id)) wait_q; 
		Queue.clear wait_q; 
		mvar := Empty(Queue.create ())
| Filled (value, wait_q) -> Scheduler.suspend (fun (cont,id) -> Queue.push (cont, v, id) wait_q)

let try_get mvar = match !mvar with
| Empty (wait_q) -> None
| _ -> Some (get mvar)

let try_put v mvar = match !mvar with
| Empty (wait_q) -> (put v mvar); true
| _ -> false

