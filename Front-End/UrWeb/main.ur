






fun ask_idris (message : string) : string = "a"







fun counter () =
  n1 <- Lib.counter ();
 (* n2 <- Lib.counter ();*)
  return <xml><body>
   (* {[n1]}, {[n2]}*)
    {[n1]}
  </body></xml>

fun main () = return <xml><body>
  {[Lib.hello ()]}<br/>
  {[Lib.important "X"]}<br/>
  <form> <submit value="Counter fun!" action={counter}/> </form>
</body></xml>
