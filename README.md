The proposed language is the ocaml functional language with a custom backend compiler (like bucklescript)
for compiling directly to eth assembly language.
	
The usage of ocaml will provide support from the ocaml ecosystem, the possibility to use coq for proof assistance 
(https://github.com/clarus/coq-of-ocaml).
	
	
A contract is implemented as an ocaml module composed of:
	
The definition of the State record.
	
Constructor: the costructor is a function that returns an initialized State. Its signature is:
		
	Contract.t -> Message.t -> State
				
				
Contract methods: all contract methods are represented as state change functions; the signature
is common for each state-change methods:
			
	Contract.t -> Message.t -> State -> State
				
Methods where the state is not changed:
	
	Contract.t -> State -> 'a


```
(* ***** Exaple 1: simple counter *)
(* Contract signature *)
module TestContract : sig
	type State
	let constructor : Contract.t -> Message.t -> State
	let increment 	: Contract.t -> Message.t -> State -> State
	let get_counter	: Contract.t -> State -> uint64
end

(* Contract implementation *)
module TestContract = struct
	type State = {
		counter: int;
	}
	
	let constructor c m =
		{ counter= 0 };;
			
	let increment c m s =
		let c' = s.counter + 1 in
		{ s with counter= c' }
	;;
	
	let get_counter c s =
		s.counter
	;;
end
```


```
(* ***** Exaple 2: deposit/reedem contract *)
(* Contract signature *)
module TestContract2 : sig
	type State
	let construct	: Contract.t -> Message.t -> State
	let deposit		: Contract.t -> Message.t -> State -> State
	let reedem		: Contract.t -> Message.t -> State -> State
	let reedem_all	: Contract.t -> Message.t -> State -> State
end
	
module TestContract2 = struct
	type State = {
		users: Map <uint64, float>
	}
	
	let constructor c m =
		{ users= Map.empty }
	;;
	
	let deposit c m s =
		let m' = 
			if Map.exists s.users m.from then
				Map.set s.users m.from (m.value + (Map.get s.users m.from))
			else
				Map.set s.users m.from m.value
		in
		{ s with users= m' }
	;;
	
	
	let redeem c m s =
		if ! (Map.exists s.users m.from) then
			failwith "No balance for this user"
		else
			if Contract.send c m.from (Map.get s.users m.from) then
				s where { users= Map.set s.users m.from 0 }
			else
				failwith "Unexcepted failed send"
	;;
	
	let reedemAll c m s =
		if m.from <> c.owner then 
			failwith "Callable only by the owner"
		else
			let u' = Map.iter (fun (k, i) -> if Contract.send c k i then 0 else i) s.users in
			{ s with users= u' }
	;;
end
```


