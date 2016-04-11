val hello : unit -> string
(* Returns the string "Hello" *)

val important : string -> string
(* Adds an exclamation mark at the end of its argument *)

val counter : unit -> transaction string
(* Every time this is called, it increments a global counter and returns the old value. *)
