
module type M = sig
	type 'a t
	val make_empty : unit -> 'a t
	val make : 'a -> 'a t
	val get : 'a t -> 'a
	val put : 'a -> 'a t -> unit
end

module type S = sig 

  module type AFLQueue = sig
    val enqueue : (unit -> unit) -> unit
    val dequeue : unit -> (unit -> unit)
    val is_empty : unit -> bool
    val range   : ?min:int -> int -> int
  end
  type 'a cont
  val context_switch : unit -> unit
  val fork : (unit->unit) -> unit
  val suspend : (('a cont * int) -> unit) -> 'a
  val resume : ('a cont * 'a * int) -> unit
  val range : ?min:int -> int -> int
  val run : (module AFLQueue) -> (unit -> unit) -> unit
  val get_id : unit -> int

end

module Make(Scheduler : S) : MVar_type = struct

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
			(mvar := Empty((Queue.create ())); value)
		else 
			(let (nvalue, waiting_f, id) = Queue.pop wait_q in
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
	| Filled (value, wait_q) -> Scheduler.suspend (fun (cont,id) -> Queue.push (v, cont) wait_q)

	let put_all v mvar = match (!mvar) with
	| Empty (wait_q) -> 
		if Queue.is_empty wait_q then 
			mvar := Filled(v, Queue.create ()) 
		else 
			let waiting_f = Queue.pop wait_q in  (* Here call the scheduler to pick which thread has to be woken up *)
			Scheduler.resume (waiting_f, v, id)
	| Filled (value, wait_q) -> Scheduler.suspend (fun (cont,id) -> Queue.push (v, cont) wait_q)

end
