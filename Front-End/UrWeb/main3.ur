style unselected



fun wrapList (x : list (xml ([Dyn = (), MakeForm = (), Body = ()]) ([]) ([]))) : xml ([Dyn = (), MakeForm = (), Body = ()]) ([]) ([]) =
 case x of
 [] => <xml></xml>
 | (y::ys) => <xml>{y} {wrapList ys}</xml>




val getBlah : xml ([Dyn = (), MakeForm = (), Body = ()]) ([]) ([]) = <xml>
<div></div>
</xml>


val fooBar : xml ([Dyn = (), MakeForm = (), Body = ()]) ([]) ([]) = <xml>
 <div style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;">
  <img style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
 </div>
</xml>



fun wrap (x : xml ([Dyn = (), MakeForm = (), Body = ()]) ([]) ([])) : xml ([Dyn = (), MakeForm = (), Body = ()]) ([]) ([]) = <xml>
 <div class={unselected}> {x} </div>
</xml>

fun wrapWith (x : xml ([Dyn = (), MakeForm = (), Body = ()]) ([]) ([])) (y : css_class) : xml ([Dyn = (), MakeForm = (), Body = ()]) ([]) ([]) = <xml>
 <div class={y}> {x} </div>
</xml>



val manyClasses : list css_class = unselected::unselected::unselected::unselected::[]

(*
fun argle (l : list css_class) (x : xml ([Dyn = (), MakeForm = (), Body = ()]) ([]) ([])) : list (xml ([Dyn = (), MakeForm = (), Body = ()]) ([]) ([])) =
 map (wrapWith x) l
*)

fun battle (x : string) : transaction page = return <xml>
 <head></head><body><button onclick = {fn _ => alert(x)}>CLICK!</button>{fooBar}{wrapWith getBlah unselected}</body></xml>


fun main () : transaction page = return <xml><head></head><body><button onclick = {fn _ => alert("hello")}>CLICK!</button>{fooBar}{wrapWith getBlah unselected}</body></xml>



